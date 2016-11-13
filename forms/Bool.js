// @flow

import { List, Record } from 'immutable';
import { Inline } from 'slate';

import { mkStructuralKey, mkAddrKey, mkText } from '../slateHelpers';
import { Ty, Lisp } from '../forms/bootstrap';
import { registerName } from '../name-registry';
import {
  hashable,
  singletonType,
  fixedType,
  noFixedImpl,
  noAddressableChildren,
  fixedRepresentation,
  dispatchEvents,
  irreducible,
  noMeta,
  trivialUnification,
  binaryFunction,
} from '../decorators';

import type { Address } from '../Address';
import type {
  ChildUpdate,
  Backspace,
  TypingAtEnd,
  EditorEvent,
} from '../events';
import type { SlatePath, SlateVal } from '../slateHelpers';
import type { Unif } from '../types';

// TODO: sooo much duplication between this and IntTy
// eslint-disable-next-line no-unused-vars
class BoolTy<A> extends Record({}) {
  slate(path: SlatePath): SlateVal {
    return new Inline({
      type: 'span',
      nodes: List([mkText('bool', mkStructuralKey(path, 0))]),
      key: mkAddrKey(this.hash, path),
    });
  }

  // TODO should this throw instead?
  // eslint-disable-next-line class-methods-use-this
  handleBackspace(evt: Backspace) {
    return evt;
  }
}

hashable(BoolTy);
singletonType(BoolTy);
fixedType(Ty.value)(BoolTy); // Bool : Ty
noFixedImpl(BoolTy);
noAddressableChildren(BoolTy);
fixedRepresentation(BoolTy);
dispatchEvents(BoolTy);
irreducible(BoolTy);
noMeta(BoolTy);
trivialUnification(BoolTy);
export { BoolTy };

// eslint-disable-next-line no-unused-vars
class Bool<A> extends Record({ b: null }) {
  slate(path: SlatePath): SlateVal {
    return new Inline({
      kind: 'inline',
      type: 'span',
      nodes: List([
        mkText(`${this.b}`, mkStructuralKey(path, 0)),
      ]),
      key: mkAddrKey(this.hash, path),
    });
  }

  // eslint-disable-next-line class-methods-use-this
  _recognizeBooleanString(str: string): ?boolean {
    return str === 'true'
      ? true
      : str === 'false'
        ? false
        : null;
  }

  handleTypingAtEnd(evt: TypingAtEnd): EditorEvent {
    const b = this._recognizeBooleanString(evt.textValue);
    return b != null ? this.mkUpdate(this.set('b', b), evt) : evt;
  }

  handleBackspace(evt: Backspace): ChildUpdate {
    const b = this._recognizeBooleanString(evt.textValue);
    if (b != null) {
      return this.mkUpdate(this.set('b', b), evt);
    } else {
      throw new Error('TODO Bool Backspace');
    }
  }
}

Bool.unifyChildren = function (
  l: Bool<Address>,
  r: Bool<Address>
): ?Bool<Unif> {
  return l.b === r.b
    ? this
    : new Bool({ b: { l, r } });
};


registerName('bool', () => BoolTy.value);
hashable(Bool);
fixedType(BoolTy.value)(Bool); // true : Bool
noAddressableChildren(Bool);
dispatchEvents(Bool);
irreducible(Bool);
noMeta(Bool);
export { Bool };

// eslint-disable-next-line no-unused-vars
class Or<A> extends Record({ l: null, r: null }) {}

const orNameAddr = registerName('or', (l, r) => (new Or({ l, r })));
hashable(Or);
fixedType(BoolTy.value)(Or); // _ || _ : Bool
fixedRepresentation(Or);
dispatchEvents(Or);
noMeta(Or);
binaryFunction(Or, ' || ', orNameAddr, (l, r) => new Bool({ b: l.b || r.b }), BoolTy, Lisp);
export { Or };

// eslint-disable-next-line no-unused-vars
class And<A> extends Record({ l: null, r: null }) {}

const andNameAddr = registerName('and', (l, r) => (new And({ l, r })));
hashable(And);
fixedType(BoolTy.value)(And); // _ && _ : Bool
fixedRepresentation(And);
dispatchEvents(And);
noMeta(And);
binaryFunction(And, ' && ', andNameAddr, (l, r) => new Bool({ b: l.b && r.b }), BoolTy, Lisp);
export { And };
