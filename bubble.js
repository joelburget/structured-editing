// @flow

import type { Document } from 'slate';

import { expand } from './Address';

import type { EditorEvent } from './events';
import type { Address } from './Address';

const addrKeyRe = /^addr_(\w*)_([0-9a-f]+)$/;

// Bubble an event emitting from address within document
export default function bubble(
  evt: EditorEvent,
  document: Document,
  address: Address
): EditorEvent {
  // hit the root -- return the event
  if (address == null) {
    return evt;
  }

  const node = document.getDescendant(address);
  const reResults = addrKeyRe.exec(node.key);
  const nextAddress = document.getParent(node).key;

  // hit a structural node -- skip over it
  if (reResults == null || reResults.length !== 3) {
    return bubble(evt, document, nextAddress);
  }

  const key = reResults[2];
  const term = expand(key);
  const nextEvt = term.handle(evt);

  return bubble(nextEvt, document, nextAddress);
}
