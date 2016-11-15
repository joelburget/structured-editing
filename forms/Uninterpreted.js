// @flow

import { List, Record } from 'immutable';
import { Inline } from 'slate';

import { mkStructuralKey, mkAddrKey, mkText } from '../slateHelpers';
import {
  hashable,
  dispatchEvents,
  noMeta,
  noAddressableChildren,
} from '../decorators';

import type {
  ChildUpdate,
  Backspace,
  TypingAtEnd,
} from '../events';
import type { SlatePath, SlateVal } from '../slateHelpers';


// TODO: decide whether this is a Form, or maybe a new classification should be
// made.
// eslint-disable-next-line no-unused-vars
export default class Uninterpreted<A> extends Record({ str: null }) {
  slate(path: SlatePath): SlateVal {
    return new Inline({
      type: 'span',
      nodes: List([mkText(this.str, mkStructuralKey(path, 0))]),
      key: mkAddrKey(this.hash, path),
    });
  }

  handleTypingAtEnd(evt: TypingAtEnd) {
    const { textValue, insertedText } = evt;
    if (insertedText === ' ' || insertedText === '(' || insertedText === ')') {
      return evt;
    } else {
      // TODO try interpreting, only bubble if it can't be handled
      return this.mkUpdate(this.set('str', textValue), evt);
    }
  }

  handleBackspace(evt: Backspace): ChildUpdate {
    const { textValue: str } = evt;
    // TODO try interpreting
    return this.mkUpdate(this.merge({ str }), evt);
  }
}

hashable(Uninterpreted);
dispatchEvents(Uninterpreted);
noMeta(Uninterpreted);
noAddressableChildren(Uninterpreted);
