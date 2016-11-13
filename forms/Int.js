// @flow

import { List, Record } from 'immutable';
import { Inline } from 'slate';

import { mkStructuralKey, mkAddrKey, mkText } from '../slateHelpers';
import { Lisp, Ty } from '../forms/bootstrap';
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
import type { Unif, Term, UnificationResult } from '../types';

let intNameAddr;

export class IntRelation {
  stitch(
    tm: Int<Address>,
    ty: IntTy<Address>
  ) {
    return [tm, ty];
  }

  accepts(tm, ty) {
    return (
      (tm instanceof Int || tm instanceof Addition || tm instanceof Subtraction) &&
      ty instanceof IntTy
    );
  }
}
IntRelation.value = new IntRelation;

class IntTyIsTyRelation {
  stitch(
    tm: IntTy<Address>,
    ty: Ty<Address>
  ): [IntTy<Address>, Ty<Address>] {
    return [tm, ty];
  }

  accepts(tm, ty) {
    return tm instanceof IntTy && ty instanceof Ty;
  }
}
IntTyIsTyRelation.value = new IntTyIsTyRelation;

// eslint-disable-next-line no-unused-vars
class IntTy<A> extends Record({}) {
  slate(path: SlatePath): SlateVal {
    return new Inline({
      type: 'span',
      nodes: List([mkText('int', mkStructuralKey(path, 0))]),
      key: mkAddrKey(this.hash, path),
    });
  }

  proposeTypingRelation() {
    return IntTyIsTyRelation.value;
  }

  // eslint-disable-next-line class-methods-use-this
  open(): Lisp<Address> { return new Lisp([intNameAddr]); }

  // TODO should this throw instead?
  // eslint-disable-next-line class-methods-use-this
  handleBackspace(evt: Backspace) {
    return evt;
  }

  // eslint-disable-next-line class-methods-use-this
  pushImpl(expectedImpl: Term<Unif>): UnificationResult {
    const value: Term<Unif> = IntTy.value;
    return expectedImpl === value
      ? { type: 'OuterSuccess', value }
      : { type: 'Failure', value: { l: IntTy.addr, r: expectedImpl.hash } };
  }
}

intNameAddr = registerName('int', () => IntTy.value);
hashable(IntTy);
singletonType(IntTy);
fixedType(Ty.value)(IntTy); // Int : Ty
noFixedImpl(IntTy);
noAddressableChildren(IntTy);
fixedRepresentation(IntTy);
dispatchEvents(IntTy);
irreducible(IntTy);
noMeta(IntTy);
trivialUnification(IntTy);
export { IntTy };

// eslint-disable-next-line no-unused-vars
class Int<A> extends Record({ i: null }) {
  slate(path: SlatePath): SlateVal {
    return new Inline({
      kind: 'inline',
      type: 'span',
      nodes: List([
        mkText(`${this.i}`, mkStructuralKey(path, 0)),
      ]),
      key: mkAddrKey(this.hash, path),
    });
  }

  proposeTypingRelation() {
    return IntRelation.value;
  }

  handleTypingAtEnd(evt: TypingAtEnd): EditorEvent {
    const { textValue } = evt;
    const num: ?number = +textValue;

    // TODO use interpretChunk?
    return textValue.trim() === textValue && !Number.isNaN(num)
      ? this.mkUpdate(this.set('i', num), evt)
      : evt;
  }

  handleBackspace(evt: Backspace): ChildUpdate {
    const { textValue: text } = evt;
    const num: ?number = +text;
    if (text.trim() === text && !Number.isNaN(num)) {
      return this.mkUpdate(this.set('i', num), evt);
    } else {
      throw new Error('TODO Int Backspace');
    }
  }
}

Int.unifyChildren = function (l: Int<Address>, r: Int<Address>): Int<Unif> {
  return l.i === r.i
    ? l
    : new Int({ k: { l, r } });
};

hashable(Int);
fixedType(IntTy.value)(Int); // 5 : Int
noAddressableChildren(Int);
dispatchEvents(Int);
irreducible(Int);
noMeta(Int);
export { Int };

// eslint-disable-next-line no-unused-vars
class Addition<A> extends Record({ l: null, r: null }) {}

const addNameAddr = registerName('add', (l, r) => (new Addition({ l, r })));
hashable(Addition);
fixedType(IntTy.value)(Addition); // _ + _ : Int
fixedRepresentation(Addition);
dispatchEvents(Addition);
noMeta(Addition);
binaryFunction(Addition, ' + ', addNameAddr, (l, r) => new Int({ i: l.i + r.i }), IntTy, Lisp);
export { Addition };

// eslint-disable-next-line no-unused-vars
class Subtraction<A> extends Record({ l: null, r: null }) {}

const subNameAddr = registerName('sub', (l, r) => (new Subtraction({ l, r })));
hashable(Subtraction);
fixedType(IntTy.value)(Subtraction); // _ - _ : Int
fixedRepresentation(Subtraction);
dispatchEvents(Subtraction);
noMeta(Subtraction);
binaryFunction(Subtraction, ' - ', subNameAddr, (l, r) => new Int({ i: l.i - r.i }), IntTy, Lisp);
export { Subtraction };
