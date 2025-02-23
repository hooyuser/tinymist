/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import { getLanguageService } from "vscode-html-languageservice/lib/esm/htmlLanguageService";
import {
  createConnection,
  InitializeParams,
  ProposedFeatures,
  TextDocuments,
  TextDocumentSyncKind,
} from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";

// Create a connection for the server. The connection uses Node's IPC as a transport.
// Also include all preview / proposed LSP features.
const connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager. The text document manager
// supports full document sync only
const documents = new TextDocuments(TextDocument);

const htmlLanguageService = getLanguageService();

connection.onInitialize((_params: InitializeParams) => {
  console.log("initialize server");
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Full,
      // Tell the client that the server supports code completion
      completionProvider: {
        triggerCharacters: [
          ..."abcdefghijklmnopqrstuvwxyz".split(""),
          ..."ABCDEFGHIJKLMNOPQRSTUVWXYZ".split(""),
        ],
        resolveProvider: false,
      },
    },
  };
});

connection.onCompletion(async (textDocumentPosition, _token) => {
  console.log(
    "provideCompletionItem server",
    textDocumentPosition.textDocument.uri,
    textDocumentPosition.position,
  );
  const document = documents.get(textDocumentPosition.textDocument.uri);
  if (!document) {
    return null;
  }

  return htmlLanguageService.doComplete(
    document,
    textDocumentPosition.position,
    htmlLanguageService.parseHTMLDocument(document),
  );
});

documents.listen(connection);
connection.listen();
