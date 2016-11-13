// @flow
/* eslint-disable no-plusplus */

import React from 'react';
import { List as ExList } from 'extendable-immutable';
import { List, Record } from 'immutable';
import { Inline } from 'slate';

import { mkNameAddr, registerName, names } from '../name-registry';
import { expand } from '../Address';
import { mkStructuralKey, mkAddrKey, mkText } from '../slateHelpers';
import {
  hashable,
  singletonType,
  fixedType,
  noAddressableChildren,
  fixedRepresentation,
  dispatchEvents,
  irreducible,
  noMeta,
  trivialUnification,
  mkUpdate,
} from '../decorators';
import ReadOnlySlate from '../components/ReadOnlySlate';
import Uninterpreted from './Uninterpreted';

import type { Address } from '../Address';
import type {
  ChildUpdate,
  Backspace,
  Close,
  TypingAtEnd,
  EditorEvent,
  ConflictToLisp,
  ResolveConflict,
  QueryMeta,
} from '../events';
import type { SlatePath, SlateVal } from '../slateHelpers';
import type { Term, Meta, Unif, UnificationResult } from '../types';
import { unify } from '../unify';

class TyIsTyRelation {
  stitch(
    tm: Ty<Address>,
    ty: Ty<Address>
  ): [Ty<Address>, Ty<Address>] {
    return [Ty.value, Ty.value];
  }

  accepts(tm, ty) {
    return (
      (tm == null || tm instanceof Ty) &&
      (ty == null || ty instanceof Ty)
    );
  }

  // eslint-disable-next-line class-methods-use-this
  pushImpl(value: Term<Unif>): UnificationResult {
    return { type: 'OuterSuccess', value };
  }

  // eslint-disable-next-line class-methods-use-this
  pullImpl(): null {
    return null;
  }
}

let tyNameAddr;
// eslint-disable-next-line no-unused-vars
export class Ty<A> extends Record({}) {
  slate(path: SlatePath): SlateVal {
    return new Inline({
      type: 'span',
      nodes: List([mkText('*', mkStructuralKey(path, 0))]),
      key: mkAddrKey(this.hash, path),
    });
  }

  proposeTypingRelation() {
    return new TyIsTyRelation;
  }

  // eslint-disable-next-line class-methods-use-this
  open(): Lisp<Address> { return new Lisp([tyNameAddr]); }
}

Ty.prototype.handleBackspace = Ty.prototype.open;
hashable(Ty);
singletonType(Ty);
fixedType(Ty.value)(Ty); // Ty : Ty
noAddressableChildren(Ty);
fixedRepresentation(Ty);
dispatchEvents(Ty);
irreducible(Ty);
noMeta(Ty);
trivialUnification(Ty);
tyNameAddr = registerName('ty', () => Ty.value);

let colonNameAddr;

// XXX acceptChildUpdate should handle bad types coming in from the child, but
// what if this is constructed with wrong types? We could construct everything
// with holes and then add children.
// eslint-disable-next-line no-unused-vars
export class Annotation<A> extends Record({ term: null, ty: null }) {
  static unifyChildren(
    l: Annotation<Address>,
    r: Annotation<Address>
  ): ?Annotation<Unif> {
    const unifiedTys = unify(l.ty, r.ty);
    const unifiedTms = unify(l.tm, r.tm);

    if (unifiedTys.type === 'Failure' || unifiedTms.type === 'Failure') {
      return null;
    }

    const { value: unifiedTy } = unifiedTys;
    const { value: unifiedTm } = unifiedTms;

    return new Record({ term: unifiedTm, ty: unifiedTy });
  }

  // XXX need to guarantee term and type match
  /*
  constructor({ term, ty }: {term: Address, ty: Address}) {
    super({ term, ty });
  }
  */

  slate(path: SlatePath): SlateVal {
    const { term, ty } = this;
    return new Inline({
      type: 'span',
      nodes: List([
        expand(term).slate(path.concat(0)),
        mkText(': ', mkStructuralKey(path, 1)),
        expand(ty).slate(path.concat(1)),
      ]),
      key: mkAddrKey(this.hash, path),
    });
  }

  open(): Lisp<Address> {
    const { term, ty } = this;
    return new Lisp([colonNameAddr, term, ty]);
  }

  // eslint-disable-next-line no-unused-vars, class-methods-use-this
  acceptChildUpdate({ from, to }: ChildUpdate) {
    // TODO
    debugger; // eslint-disable-line
  }

  // $FlowFixMe: TODO figure out how this works
  pushType(expectedTy: Address): UnificationResult { // eslint-disable-line
    const { type: tag, value } = unify(expand(this.ty), expand(expectedTy)); // eslint-disable-line
    debugger; // eslint-disable-line
  }

  pullType(): Address { return this.type; }

  normalize() {
    return this.term.normalize();
  }
}

Annotation.prototype.handleBackspace = Annotation.prototype.open;
hashable(Annotation);
fixedRepresentation(Annotation);
dispatchEvents(Annotation);
noMeta(Annotation);
colonNameAddr = registerName(':', (term, ty) => new Annotation({ term, ty }));

let conflictNameAddr;

// eslint-disable-next-line no-unused-vars
export class Conflict<A> extends Record({ value: null, outerTy: null, innerTy: null }) {
  // XXX
  static unifyChildren() {
  }

  slate(path: SlatePath): SlateVal {
    const v = expand(this.value);
    return new Inline({
      type: 'conflict',
      nodes: List([v.slate(path.concat(0))]),
      key: mkAddrKey(this.hash, path),
    });
  }

  meta(): Array<Meta> {
    const { value, innerTy, outerTy } = this;
    return [{
      address: this.hash,
      message: (
        <div>
          [<ReadOnlySlate address={value} />]
          -&gt; <ReadOnlySlate address={innerTy} />
          | &lt;- <ReadOnlySlate address={outerTy} />
        </div>
      ),
      choices: [
        {
          choice: 'take inner',
          event: {
            type: 'ResolveConflict',
            tyChoice: innerTy,
          },
        },
        {
          choice: 'take outer',
          event: {
            type: 'ResolveConflict',
            tyChoice: outerTy,
          },
        },
      ],
    }];
  }

  pullType(): Address { return this.outerTy; }

  open(): Lisp<Address> {
    const { value, innerTy, outerTy } = this;
    return new Lisp([conflictNameAddr, value, innerTy, outerTy]);
  }

  conflictToLisp(evt: ConflictToLisp): ChildUpdate {
    const child = expand(this.value);
    const childUpdate = child.open();
    return mkUpdate(this.hash, childUpdate.to, evt);
  }

  resolveConflict(evt: ResolveConflict): ChildUpdate { // eslint-disable-line consistent-return
    const { tyChoice } = evt;

    // Isn't this kind of just an optimization of the following case?
    if (tyChoice === this.innerTy) {
      return {
        type: 'ChildUpdate',
        from: this.hash,
        to: this.value,
      };
    } else {
      // TODO don't these problems all have to be reified as conflicts in the
      // result
      const todo = expand(this.value).pushType(tyChoice); // eslint-disable-line
      debugger; // eslint-disable-line
      // if (to.hash === this.hash) {
      //   console.log("I'm sorry, Dave. I'm afraid I can't do that.");
      // }
      // return { type: 'ChildUpdate', from: this.hash, to };
    }
  }

  queryMeta({ type, soFar }: QueryMeta): QueryMeta {
    return { type, soFar: soFar.concat(this.hash) };
  }

  // TODO should this throw instead?
  // eslint-disable-next-line class-methods-use-this
  handleBackspace(evt: Backspace) {
    return evt;
  }

  acceptChildUpdate(evt: ChildUpdate) {
    const { from, to } = evt;
    const { value, innerTy, outerTy } = this;

    let newVal;
    if (from === value) {
      // pullType: trying to connect value.type to outerTy
      const newInnerTy = expand(to).pullType();
      newVal = newInnerTy === outerTy
        ? expand(to)
        : this.merge({ value: to, innerTy: newInnerTy });
    } else if (from === innerTy) {
      // TODO
      // pullType: trying to connect innerTy to outerTy
      const question = expand(value).pullType() === to; // eslint-disable-line no-unused-vars
      newVal = to === outerTy
        ? expand(to)
        : this.merge({ innerTy: to });
      debugger; // eslint-disable-line no-debugger
    } else if (from === outerTy) {
      // TODO this one feels more difficult / thought-provoking.
      //
      // How do we get notified that the outer ty has updated? Conceptually
      // it's "outside" this node. More to the point, how do we get notified
      // when one call to pullType returns #1234 and the next returns #abcd?
      //
      // pullType: trying to connect innerTy to outerTy
      newVal = to === innerTy
        ? expand(value)
        : this.merge({ outerTy: to });
    }

    return this.mkUpdate(newVal, evt);
  }
}

conflictNameAddr = registerName('conflict', (value, innerTy, outerTy) => new Conflict({ value, innerTy, outerTy }));
hashable(Conflict);
fixedRepresentation(Conflict);
dispatchEvents(Conflict);
irreducible(Conflict);

// This represents an *open* lisp expression
// eslint-disable-next-line no-unused-vars
export class Lisp<A> extends ExList {
  acceptChildUpdate(evt: ChildUpdate): ChildUpdate {
    const { from, to } = evt;
    const newVal = this.update(
      arr => arr.map(addr => (addr === from ? to : addr))
    );
    return this.mkUpdate(newVal, evt);
  }

  handleClose(evt: Close): ChildUpdate {
    const result = this.close();
    return this.mkUpdate(result, evt);
  }

  close(): Address {
    const [nameAddr, ...args] = this.toArray();
    const numArgs = args.length;

    const name = (expand(nameAddr): Uninterpreted<*>).str;
    const registryEntry = names[name];
    if (registryEntry != null) {
      if (registryEntry.handler.length !== numArgs) {
        throw new
          Error('TODO figure out how to handle mismatching arg lengths');
      }

      return registryEntry.handler(args);
    }

    throw new Error('TODO entry not found in name registry');
  }

  handleTypingAtEnd(evt: TypingAtEnd): EditorEvent {
    const { insertedText } = evt;
    if (insertedText === ')') {
      // close out this lisp
      // TODO this event isn't a close event -- should we wrap it in one or
      // something?
      return this.handleClose(evt);
    } else if (insertedText === ' ') {
      const newChildren = this.push(mkNameAddr(''));
      return this.mkUpdate(newChildren, evt);
    } else {
      // inserted some non-control text (XXX what about '('?)
      // XXX wont this be handled by the child?

      throw new Error('XXX unimplemented');
    }
  }

  // eslint-disable-next-line class-methods-use-this, no-unused-vars
  handleBackspace(evt: Backspace): EditorEvent {
    throw new Error('TODO Lisp.handleBackspace');
  }

  // this will make this thing not a lisp anymore
  // $FlowFixMe
  pushType(expectedTy: Term<Unif>): UnificationResult {
    const [nameAddr, ...args] = this.toArray();

    /*
    if (expectedTy instanceof IntTy && args.length === 2) {
      // XXX could also be subtraction
      const [l, r] = args;
      const value = new Addition({ l, r });
      return { type: 'OuterSuccess', value };
    }
    */
    console.log({ expectedTy, nameAddr, args });
    debugger; // eslint-disable-line no-debugger
  }

  // We can't know the type here.
  // eslint-disable-next-line class-methods-use-this
  pullType(): null { return null; }

  slate(path: SlatePath): SlateVal {
    let i = 0;
    let nodes = [
      mkText('(', mkStructuralKey(path, i++)),
    ];

    this.forEach((addr) => {
      nodes.push(
        expand(addr).slate(path.concat(i++)),
        mkText(' ', mkStructuralKey(path, i++))
      );
    });

    // we added one more space than necessary
    nodes.splice(-1, 1,
      new Inline({
        type: 'closeParen',
        nodes: List([mkText(')', mkStructuralKey(path, i++))]),
        key: mkStructuralKey(path, i++),
      })
    );
    nodes = List(nodes);

    return new Inline({ type: 'span', nodes, key: mkAddrKey(this.hash, path) });
  }
}

hashable(Lisp);
dispatchEvents(Lisp);
noMeta(Lisp);
trivialUnification(Lisp);

// Application is very important:
// * It's where value and computation collide to annihilate
// * It's where we connect new sockets
//
// This is a list of two terms: one must be a value, the other a computation.
// But we don't prescribe an order.
export class Application<A> extends ExList {
  slate(path: SlatePath): SlateVal {
    return new Inline({
      type: 'span',
      nodes: this.map((tm, i) => expand(tm).slate(path.concat(i))),
      key: mkStructuralKey(path, 0),
    });
  }

  annihilate(): Address {
    const a0: Address = this.get(0);
    const a1: Address = this.get(1);
    const t0: Term<Address> = expand(a0);
    const t1: Term<Address> = expand(a1);

    return t0.modality === 'computation'
      ? t0.normalize(a1)
      : t1.normalize(a0);
  }
}

hashable(Application);

// Refs are mutable references into the content-addressed immutable store.
// They're analogous to refs in Git. They realize the concept of a definition
// which can be updated, as opposed to a commit, which represents a definition
// at one point in time.
//
// Right now they're thrown together in a global namespace, but in the future a
// third component will be added -- signer / signature -- a public key /
// signature pair. The public key is your identity, as an individual or group
// (this could definitely be a group / ring signature or even a threshold
// signature). This way there's no global namespace -- you're free to use
// whichever name you like, but only under your key, including path puns like
// "lodash/map". And your namespace is yours as much as your key is yours.
//
// Users shouldn't have to deal with public keys very often -- instead they
// deal with "alice", "google", "some package maintainers".
//
// Additionally, there should be some notion of history which isn't built in
// right now, so we can see how to fast-forward a stale ref to a newer version.
// Probably, we should add a new notion of Commit / DefinitionVersion, which is
// just an Address, but with a delta and pointer back to the previous Commit.
//
// One more note -- there should probably be some extra information about the
// intention of this ref. These should mean different things, even if they all
// resolve to the same address right now:
//
// * foo#abcd   <- pinned to this version
// * foo@head   <- always wants the latest foo
// * foo@stable
export class Ref extends Record({ name: null, refHash: null }) {
  // eslint-disable-next-line no-unused-vars
  static unifyChildren(l: Ref, r: Ref) {
    // XXX finish
    debugger; // eslint-disable-line
  }

  slate(path: SlatePath): SlateVal {
    const { name, refHash }: { name: ?Address, refHash: Address } = this;
    const name_: string = name == null
      ? ''
      : (expand(name) : Uninterpreted<*>).str;
    const text: string = `${name_}#${refHash.slice(0, 4)}`;

    return new Inline({
      type: 'span',
      nodes: List([mkText(text, mkStructuralKey(path, 0))]),
      key: mkAddrKey(this.hash, path),
    });
  }

  pullType() { return expand(this.refHash.str).pullType(); }

  handleBackspace(evt: Backspace): EditorEvent {
    return this.mkUpdate(
      new Lisp([names.workspace.addr]),
      evt
    );
  }

  acceptChildUpdate({ from, to }: ChildUpdate): EditorEvent {
    if (this.name === from) return this.set('name', to);
    if (this.refHash === from) return this.set('refHash', to);
    throw new Error('Ref: unexpected child update');
  }

  // TODO fix the expand(expand(...)) problem
  normalize() {
    const strAddress: Uninterpreted<*> = expand(this.refHash);
    return expand(strAddress.str).normalize();
  }
}

// eslint-disable-next-line no-unused-vars
registerName('ref', (name, addr) => {
  throw new Error('TODO handleTypingAtEnd ) ref');
});
hashable(Ref);
fixedRepresentation(Ref);
// noAddressableChildren(Ref);
dispatchEvents(Ref);
noMeta(Ref);
