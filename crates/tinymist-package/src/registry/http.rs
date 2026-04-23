//! Http registry for tinymist.

use std::io::{Cursor, ErrorKind, Read};
use std::path::{Path, PathBuf};
use std::sync::{Arc, OnceLock};
use std::time::{SystemTime, UNIX_EPOCH};

use parking_lot::Mutex;
use reqwest::Certificate;
use reqwest::blocking::Response;
use tinymist_std::ImmutPath;
use typst::diag::{PackageResult, StrResult, eco_format};
use typst::syntax::package::{PackageVersion, VersionlessPackageSpec};

use crate::registry::{PREVIEW_NS, PackageIndexEntry, PackageSpecExt};

use super::{
    DEFAULT_REGISTRY, DummyNotifier, Notifier, PackageError, PackageRegistry, PackageSpec,
};

/// The http package registry for typst.ts.
pub struct HttpRegistry {
    /// The path at which local packages (`@local` packages) are stored.
    package_path: Option<ImmutPath>,
    /// The path at which non-local packages (`@preview` packages) should be
    /// stored when downloaded.
    package_cache_path: Option<ImmutPath>,
    /// lazily initialized package storage.
    storage: OnceLock<PackageStorage>,
    /// The path to the certificate file to use for HTTPS requests.
    cert_path: Option<ImmutPath>,
    /// The notifier to use for progress updates.
    notifier: Arc<Mutex<dyn Notifier + Send>>,
    // package_dir_cache: RwLock<HashMap<PackageSpec, Result<ImmutPath, PackageError>>>,
}

impl Default for HttpRegistry {
    fn default() -> Self {
        Self {
            notifier: Arc::new(Mutex::<DummyNotifier>::default()),
            cert_path: None,
            package_path: None,
            package_cache_path: None,

            storage: OnceLock::new(),
            // package_dir_cache: RwLock::new(HashMap::new()),
        }
    }
}

impl std::ops::Deref for HttpRegistry {
    type Target = PackageStorage;

    fn deref(&self) -> &Self::Target {
        self.storage()
    }
}

impl HttpRegistry {
    /// Create a new registry.
    pub fn new(
        cert_path: Option<ImmutPath>,
        package_path: Option<ImmutPath>,
        package_cache_path: Option<ImmutPath>,
    ) -> Self {
        Self {
            cert_path,
            package_path,
            package_cache_path,
            ..Default::default()
        }
    }

    /// Get `typst-kit` implementing package storage
    pub fn storage(&self) -> &PackageStorage {
        self.storage.get_or_init(|| {
            PackageStorage::new(
                self.package_cache_path
                    .clone()
                    .or_else(|| Some(dirs::cache_dir()?.join(DEFAULT_PACKAGES_SUBDIR).into())),
                self.package_path
                    .clone()
                    .or_else(|| Some(dirs::data_dir()?.join(DEFAULT_PACKAGES_SUBDIR).into())),
                self.cert_path.clone(),
                self.notifier.clone(),
            )
        })
    }

    /// Get local path option
    pub fn local_path(&self) -> Option<ImmutPath> {
        self.storage().package_path().cloned()
    }

    /// Get data & cache dir
    pub fn paths(&self) -> Vec<ImmutPath> {
        let data_dir = self.storage().package_path().cloned();
        let cache_dir = self.storage().package_cache_path().cloned();
        data_dir.into_iter().chain(cache_dir).collect::<Vec<_>>()
    }

    /// Set list of packages for testing.
    pub fn test_package_list(&self, f: impl FnOnce() -> Vec<PackageIndexEntry>) {
        self.storage().index.get_or_init(f);
    }
}

impl PackageRegistry for HttpRegistry {
    fn resolve(&self, spec: &PackageSpec) -> Result<ImmutPath, PackageError> {
        self.storage().prepare_package(spec)
    }

    fn packages(&self) -> &[PackageIndexEntry] {
        self.storage().download_index()
    }
}

/// The default packages sub directory within the package and package cache
/// paths.
pub const DEFAULT_PACKAGES_SUBDIR: &str = "typst/packages";

/// Holds information about where packages should be stored and downloads them
/// on demand, if possible.
pub struct PackageStorage {
    /// The path at which non-local packages should be stored when downloaded.
    package_cache_path: Option<ImmutPath>,
    /// The path at which local packages are stored.
    package_path: Option<ImmutPath>,
    /// The downloader used for fetching the index and packages.
    cert_path: Option<ImmutPath>,
    /// The cached index of the preview namespace.
    index: OnceLock<Vec<PackageIndexEntry>>,
    notifier: Arc<Mutex<dyn Notifier + Send>>,
}

impl PackageStorage {
    /// Creates a new package storage for the given package paths.
    /// It doesn't fallback directories, thus you can disable the related
    /// storage by passing `None`.
    pub fn new(
        package_cache_path: Option<ImmutPath>,
        package_path: Option<ImmutPath>,
        cert_path: Option<ImmutPath>,
        notifier: Arc<Mutex<dyn Notifier + Send>>,
    ) -> Self {
        Self {
            package_cache_path,
            package_path,
            cert_path,
            notifier,
            index: OnceLock::new(),
        }
    }

    /// Returns the path at which non-local packages should be stored when
    /// downloaded.
    pub fn package_cache_path(&self) -> Option<&ImmutPath> {
        self.package_cache_path.as_ref()
    }

    /// Returns the path at which local packages are stored.
    pub fn package_path(&self) -> Option<&ImmutPath> {
        self.package_path.as_ref()
    }

    /// Make a package available in the on-disk cache.
    pub fn prepare_package(&self, spec: &PackageSpec) -> PackageResult<ImmutPath> {
        let subdir = format!("{}/{}/{}", spec.namespace, spec.name, spec.version);

        if let Some(packages_dir) = &self.package_path {
            let dir = packages_dir.join(&subdir);
            if dir.exists() {
                return Ok(dir.into());
            }
        }

        if let Some(cache_dir) = &self.package_cache_path {
            let dir = cache_dir.join(&subdir);
            if dir.exists() {
                return Ok(dir.into());
            }

            // Download from network if it doesn't exist yet.
            if spec.is_preview() {
                self.download_package(spec, &dir)?;
                if dir.exists() {
                    return Ok(dir.into());
                }
            }
        }

        Err(PackageError::NotFound(spec.clone()))
    }

    /// Try to determine the latest version of a package.
    pub fn determine_latest_version(
        &self,
        spec: &VersionlessPackageSpec,
    ) -> StrResult<PackageVersion> {
        if spec.is_preview() {
            // For `@preview`, download the package index and find the latest
            // version.
            self.download_index()
                .iter()
                .filter(|entry| entry.package.name == spec.name)
                .map(|entry| entry.package.version)
                .max()
                .ok_or_else(|| eco_format!("failed to find package {spec}"))
        } else {
            // For other namespaces, search locally. We only search in the data
            // directory and not the cache directory, because the latter is not
            // intended for storage of local packages.
            let subdir = format!("{}/{}", spec.namespace, spec.name);
            self.package_path
                .iter()
                .flat_map(|dir| std::fs::read_dir(dir.join(&subdir)).ok())
                .flatten()
                .filter_map(|entry| entry.ok())
                .map(|entry| entry.path())
                .filter_map(|path| path.file_name()?.to_string_lossy().parse().ok())
                .max()
                .ok_or_else(|| eco_format!("please specify the desired version"))
        }
    }

    /// Get the cached package index without network access.
    pub fn cached_index(&self) -> Option<&[PackageIndexEntry]> {
        self.index.get().map(Vec::as_slice)
    }

    /// Download the package index. The result of this is cached for efficiency.
    pub fn download_index(&self) -> &[PackageIndexEntry] {
        self.index.get_or_init(|| {
            let url = format!("{DEFAULT_REGISTRY}/preview/index.json");

            threaded_http(&url, self.cert_path.as_deref(), |resp| {
                let reader = match resp.and_then(|r| r.error_for_status()) {
                    Ok(response) => response,
                    Err(err) => {
                        // todo: silent error
                        log::error!("Failed to fetch package index: {err} from {url}");
                        return vec![];
                    }
                };

                let mut entries: Vec<PackageIndexEntry> = match serde_json::from_reader(reader) {
                    Ok(entry) => entry,
                    Err(err) => {
                        log::error!("Failed to parse package index: {err} from {url}");
                        return vec![];
                    }
                };
                for entry in &mut entries {
                    entry.namespace = PREVIEW_NS.into();
                }

                entries
            })
            .unwrap_or_default()
        })
    }

    /// Download a package over the network.
    ///
    /// The package is first unpacked into a temporary directory adjacent to the
    /// final destination and only moved into place after the unpack completed
    /// successfully. This prevents interrupted downloads or process crashes
    /// from leaving a partial package in the cache directory.
    ///
    /// # Panics
    /// Panics if the package spec namespace isn't `preview`.
    pub fn download_package(&self, spec: &PackageSpec, package_dir: &Path) -> PackageResult<()> {
        assert!(spec.is_preview(), "only preview packages can be downloaded");

        let base_dir = package_dir.parent().ok_or_else(|| {
            PackageError::Other(Some(eco_format!(
                "cannot determine package directory parent for {package_dir:?}"
            )))
        })?;
        let tempdir = TemporaryPackageDir::create(base_dir.join(format!(
            ".tmp-{}-{}",
            spec.version,
            temporary_dir_suffix(),
        )))
        .map_err(|err| {
            PackageError::Other(Some(eco_format!(
                "failed to create temporary package directory: {err}"
            )))
        })?;

        let url = format!(
            "{DEFAULT_REGISTRY}/preview/{}-{}.tar.gz",
            spec.name, spec.version
        );

        self.notifier.lock().downloading(spec);
        threaded_http(&url, self.cert_path.as_deref(), move |resp| {
            let mut reader = match resp.and_then(|r| r.error_for_status()) {
                Ok(response) => response,
                Err(err) if matches!(err.status().map(|s| s.as_u16()), Some(404)) => {
                    return Err(PackageError::NotFound(spec.clone()));
                }
                Err(err) => return Err(PackageError::NetworkFailed(Some(eco_format!("{err}")))),
            };

            let mut data = Vec::new();
            reader
                .read_to_end(&mut data)
                .map_err(|err| PackageError::NetworkFailed(Some(eco_format!("{err}"))))?;

            let decompressed = flate2::read::GzDecoder::new(Cursor::new(data));
            tar::Archive::new(decompressed)
                .unpack(tempdir.path())
                .map_err(|err| PackageError::MalformedArchive(Some(eco_format!("{err}"))))?;

            tempdir.persist(package_dir)
        })
        .ok_or_else(|| PackageError::Other(Some(eco_format!("cannot spawn http thread"))))?
    }
}

struct TemporaryPackageDir(PathBuf);

impl TemporaryPackageDir {
    fn create(path: PathBuf) -> std::io::Result<Self> {
        std::fs::create_dir_all(&path)?;
        Ok(Self(path))
    }

    fn path(&self) -> &Path {
        &self.0
    }

    fn persist(&self, package_dir: &Path) -> PackageResult<()> {
        match std::fs::rename(&self.0, package_dir) {
            Ok(()) => Ok(()),
            Err(err)
                if matches!(
                    err.kind(),
                    ErrorKind::AlreadyExists | ErrorKind::DirectoryNotEmpty
                ) || package_dir.exists() =>
            {
                Ok(())
            }
            Err(err) => Err(PackageError::Other(Some(eco_format!(
                "failed to move downloaded package directory: {err}"
            )))),
        }
    }
}

impl Drop for TemporaryPackageDir {
    fn drop(&mut self) {
        std::fs::remove_dir_all(&self.0).ok();
    }
}

fn temporary_dir_suffix() -> String {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    format!("{}-{nanos:x}", std::process::id())
}

pub(crate) fn threaded_http<T: Send + Sync>(
    url: &str,
    cert_path: Option<&Path>,
    f: impl FnOnce(Result<Response, reqwest::Error>) -> T + Send + Sync,
) -> Option<T> {
    std::thread::scope(|s| {
        s.spawn(move || {
            let client_builder = reqwest::blocking::Client::builder();

            let client = if let Some(cert_path) = cert_path {
                let cert = std::fs::read(cert_path)
                    .ok()
                    .and_then(|buf| Certificate::from_pem(&buf).ok());
                if let Some(cert) = cert {
                    client_builder.add_root_certificate(cert).build().unwrap()
                } else {
                    client_builder.build().unwrap()
                }
            } else {
                client_builder.build().unwrap()
            };

            f(client.get(url).send())
        })
        .join()
        .ok()
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn temporary_package_dir_persists_atomically() {
        let root = std::env::temp_dir().join(format!(
            "tinymist-package-http-test-{}",
            temporary_dir_suffix()
        ));
        let base_dir = root.join("preview").join("example");
        let final_dir = base_dir.join("0.1.0");

        let tempdir = TemporaryPackageDir::create(base_dir.join(format!(
            ".tmp-{}",
            temporary_dir_suffix()
        )))
        .unwrap();
        std::fs::write(tempdir.path().join("lib.typ"), "hello").unwrap();

        tempdir.persist(&final_dir).unwrap();

        assert!(final_dir.is_dir());
        assert_eq!(std::fs::read_to_string(final_dir.join("lib.typ")).unwrap(), "hello");

        std::fs::remove_dir_all(root).ok();
    }
}
