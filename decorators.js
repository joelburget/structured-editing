/* eslint-disable no-param-reassign, func-names */

import { List } from 'immutable';
import { Inline } from 'slate';

import { expand, hashObj, set as setAddr } from './Address';
import { mkStructuralKey, mkAddrKey, mkText } from './slateHelpers';

import type { Address } from './Address';
import type {
  ChildUpdate,
  EditorEvent,
  Open,
  Event,
} from './events';
import type { Term, Unif, UnificationResult } from './types';
import type { SlatePath, SlateVal } from './slateHelpers';

export function mkUpdate(
  from: Address,
  to: Address,
  provenance: EditorEvent
): ChildUpdate {
  return { type: 'ChildUpdate', from, to, provenance };
}

// => Class<T & Hashable>
export function hashable<T>(target: T): void {
  // $FlowFixMe: https://github.com/facebook/flow/issues/285
  Object.defineProperty(target.prototype, 'hash', {
    get() {
      const obj = {
        constructor: target.name,
        obj: this.toJS(),
      };
      return hashObj(obj).hash;
    },
  });

  target.prototype.mkUpdate = function (
    to: Term<Address>,
    evt: Event
  ): ChildUpdate {
    // TODO: find alternative to the setAddr hack here?
    return mkUpdate(this.hash, setAddr(to), evt);
  };
}

export function singletonType(target) {
  target.value = new target();
  target.addr = setAddr(target.value);
}

export function noPullType(target) {
  target.prototype.pullType = function (): null { return null; };
}

export function noPullImpl(target) {
  target.prototype.pullImpl = function (): null { return null; };
}

// XXX this needs to be revamped
export function fixedType(ty: Term<Address>) {
  return function (target) {
    /*
    target.prototype.pushType = function (
      pushedTy: Term<Unif>
    ): UnificationResult {
      return unify(ty, pushedTy);
    };

    target.prototype.pullType = function (): Term<Unif> {
      return ty.hash;
    };
    */
  };
}

export function noFixedImpl(target) {
  target.prototype.pullImpl = function (): ?Term<Unif> { return null; };
}

// Must be a singleton!
export function trivialUnification(target) {
  // XXX understand if unifyChildren is necessary given pushType / pullType
  // eslint-disable-next-line no-unused-vars
  target.unifyChildren = function (l: Term<Address>, r: Term<Address>): Term<Unif> {
    return target.value;
  };
}

// In other words, stub out this definition that should never be exercised
export function noAddressableChildren(target) {
  // eslint-disable-next-line no-unused-vars
  target.prototype.acceptChildUpdate = function (args) { return this; };
  target.prototype.mapSubterms = function(f: (a: A) => B): Term<B> {
    return this;
  };
}

// This thing's representation is fixed -- it accepts no typing. Its children
// can still accept typing, though.
export function fixedRepresentation(target) {
  target.prototype.handleTypingAtEnd = function (evt) {
    return evt;
  };
}

export function dispatchEvents(target) {
  target.prototype.handle = function (evt: EditorEvent): EditorEvent {
    if (evt.type === 'TypingAtEnd') {
      return this.handleTypingAtEnd(evt);
    } else if (evt.type === 'Backspace') {
      return this.handleBackspace(evt);
    } else if (evt.type === 'ChildUpdate') {
      return this.acceptChildUpdate(evt);
    } else if (evt.type === 'ResolveConflict') {
      // TODO figure out how to deal with only `Conflict` handling this
      return this.resolveConflict(evt);
    } else if (evt.type === 'Close') {
      return this.mkUpdate(this.close(), evt);
    } else if (evt.type === 'Open') {
      return this.mkUpdate(this.open(), evt);
    } else if (evt.type === 'ConflictToLisp') {
      return this.conflictToLisp(evt);
    } else if (evt.type === 'QueryMeta') {
      return typeof this.queryMeta !== 'undefined'
        ? this.queryMeta(evt)
        : evt;
    } else if (evt.type === 'Substitute') {
      return {
        type: 'ChildUpdate',
        from: evt.from,
        to: evt.to,
        provenance: evt,
      };
    } else {
      throw new Error('Unhandled event!');
    }
  };
}

// Normalize can never make progress on this class alone.
//
// In other words, this is an introduction form. It's stuck without a
// corresponding elimination. TODO: use this intuition, introduce notion of
// introduction / elimination.
export function irreducible(target) {
  target.prototype.normalize = function (): Address {
    return this.hash;
  };
}

export function noMeta(target) {
  target.prototype.meta = function (): Array<Meta> { return []; };
}

export function mapSubtermsIsMap(target) {
  target.prototype.mapSubterms = function(f: (a: A) => B): Term<B> {
    return this.map(f);
  };
}

// * Both arguments must have the same type -- a singleton
// * Result type must be fixed
// * Fun takes and returns boxed values
// * Children named `l` and `r` on the target
// HACK: takes Lisp as an argument since `decorators` is prior to `bootstrap`
export function binaryFunction(target, appearance, nameAddr, fun, childrenTy, Lisp) {
  target.unifyChildren = function (
    l: Term<Address>,
    // eslint-disable-next-line no-unused-vars
    r: Term<Address>
  ): Term<Unif> {
    // normalize if possible -- otherwise just unify children
    throw new Error('TODO binaryFunction.unifyChildren');
  };

  target.prototype.slate = function (path: SlatePath): SlateVal {
    const l = expand(this.l);
    const r = expand(this.r);

    return new Inline({
      type: 'span',
      nodes: List([
        l.slate(path.concat(0)),
        mkText(appearance, mkStructuralKey(path, 0)),
        r.slate(path.concat(1)),
      ]),
      key: mkAddrKey(this.hash, path),
    });
  };

  target.prototype.open = function (evt: Open): ChildUpdate {
    const { l, r } = this;
    return this.mkUpdate(new Lisp([nameAddr, l, r]), evt);
  };

  target.prototype.handleBackspace = target.prototype.open;

  target.prototype.normalize = function () {
    const [l, r] = [this.l, this.r]
      .map(expand)
      .map(tm => expand(tm.normalize()));

    // TODO is this kosher, this instanceof check? Should we only be able to
    // ask the terms for their type? That's problematic because their type
    // doesn't tell if they're fully reduced or not!
    return setAddr(
      l instanceof childrenTy && r instanceof childrenTy
        ? fun(l, r) // new Bool({ b: l_.b || r_.b })
        : new target({ l: l.hash, r: r.hash })
    );
  };

  target.prototype.acceptChildUpdate = function (evt: ChildUpdate): ChildUpdate {
    const { from, to } = evt;
    // XXX Assuming a ChildUpdate can't change the type of the child. We need
    // some way to account for that. Attach to ChildUpdate if types are flowing
    // up?

    return this.mkUpdate(
      this.mapSubterms(addr => (addr === from ? to : addr)),
      evt
    );
  };

  target.prototype.mapSubterms = function(f: (a: A) => B): Term<B> {
    return this.map(f);
  };
}
