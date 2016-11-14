// @flow

import { set as setAddr, expand } from './Address';
import { Application } from './forms/bootstrap';

import type { Term, Unif, TypecheckResult, UnificationResult } from './types';
import type { Address } from './Address';


export class TypecheckFailure {}

export class BadRelation extends TypecheckFailure {
  constructor(relation, tm, ty) {
    super();
    this.relation = relation;
    this.tm = tm;
    this.ty = ty;
  }
}

export function typecheck(tm: ?Term<Unif>, ty: ?Term<Unif>, relation): TypecheckResult {
  if (tm == null && ty == null) {
    throw new Error("can't typecheck if both are null");
  }

  // TODO this is hacky
  if (tm instanceof Application) tm = normalize(setAddr(tm));
  if (ty instanceof Application) ty = normalize(setAddr(ty));

  if (relation.accepts(tm, ty)) {
    return relation.stitch(tm, ty);
  } else {
    return new BadRelation(relation, tm, ty);
  }
}

type NominalBinding = {
  type: 'NominalBinding';
  closing: Map<string, Address>;
};

type PositionalBinding = {
  type: 'PositionalBinding';
  closing: List<Address>;
};

type AtomicBinding = {
  type: 'AtomicBinding';
  closing: Address;
};

type Binding = NominalBinding | PositionalBinding | AtomicBinding;

// export function close(body: Term<Address>, binding: Binding): Term<Address> {
// }

// export function open(tm: Term<Address>)

function normalize(addr: Address): Term<Address> {
  let curVal: Term<Address> = expand(addr);
  while (curVal instanceof Application) {
    curVal = expand(curVal.annihilate());
  }
  return curVal;
}

// TODO: we need to be careful to not throw away information! A unification
// result really needs to say what it did! Track the unifier. Similarly / as
// part of the same work, normalization needs to track in the same way.
//
// One more related note (this is the wrong place for it) -- each form's
// normalization needs a notion of input / output positions. The input positions
// should be values coming in, but the output positions need not be.
//
// Thinking: Vaguely tied in with clojure.spec generativity.
export function unify(l: ?Address, r: ?Address): UnificationResult {
  // learn from the side we have, if any
  if (l == null && r == null) {
    throw new Error('need at least one argument to be non-null in unify');
  } else if (l == null || r == null) {
    return { type: 'OuterSuccess', value: expand(l || r) };

  } else {
    let l_ = normalize(l);
    let r_ = normalize(r);

    if (l_.constructor === r_.constructor &&
        l_.constructor.hasOwnProperty('unifyChildren')) {
      const value = l_.constructor
        .unifyChildren(l_, r_)
        .map(v => typeof v.l === "string" && typeof v.r === "string"
          // two different addresses -- unify them
          ? unify(v.l, v.r)
          // one answer
          : v
          );
      return { type: 'OuterSuccess', value };
    }

    return { type: 'Failure', value: { l, r } };
  }
}
