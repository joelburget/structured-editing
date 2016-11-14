// @flow

import { Map as ExMap } from 'extendable-immutable';
import { List, Record } from 'immutable';
import { Block, Inline } from 'slate';

import { Ty } from './bootstrap';
import { hashable } from '../decorators';
import { expand } from '../Address';
import { typecheck } from '../unify';
import { tableRow, tableCell, mkStructuralKey, mkAddrKey, mkText }
  from '../slateHelpers';
import recordLikeRender from '../recordLikeRender';

import type { Address } from '../Address';
import type { SlatePath, SlateVal } from '../slateHelpers';
import type { Term, Unif } from '../types';
import type Uninterpreted from './Uninterpreted';

export class RecRelation {
  stitch(
    tm: Rec<Address>,
    ty: RecTy<Address>
  ): [Rec<Unif>, RecTy<Unif>] {
    const l_ = tm.map(l => ({ l }));
    const r_ = ty.map(r => ({ r }));
    const zipMerged = l_.mergeWith(({ l }, { r }) => ({ l, r }), r_);

    let accumTm = tm;
    let accumTy = ty;
    zipMerged.forEach(({ l, r }, i) => {
      // if one's null try to generate the other
      if (l == null || r == null) {
        accumTm.set(i, l);
        accumTy.set(i, r);
      } else {
        const l_ = expand(l);
        const r_ = expand(r);
        const [tmChild, tyChild] = typecheck(l_, r_, l_.proposeTypingRelation())
        accumTm.set(i, tmChild);
        accumTy.set(i, tyChild);
      }
    });

    return [accumTm, accumTy];
  }

  /*
  // eslint-disable-next-line class-methods-use-this
  pullType() { return RecTy; }

  // eslint-disable-next-line class-methods-use-this
  pullImpl() { return Rec; }
  */

  accepts(tm: Term<Address>, ty: Term<Address>): bool {
    return tm instanceof Rec && ty instanceof RecTy;
  }
}
RecRelation.value = new RecRelation;

// TODO there should be a relation factory (?) for this
export class RecTyRelation {
  stitch(
    tm: RecTy<Address>,
    ty: Ty<Address> // eslint-disable-line no-unused-vars
  ): [RecTy<Address>, Ty<Address>] {
    return [tm, ty];
  }

  accepts(tm: Term<Address>, ty: Term<Address>) {
    return tm instanceof RecTy && ty instanceof Ty;
  }
}
RecTyRelation.value = new RecTyRelation;

// eslint-disable-next-line no-unused-vars
export class Rec<A> extends ExMap {
  modality = 'value';

  proposeTypingRelation() {
    return RecRelation.value;
  }

  slate(path: SlatePath): SlateVal {
    return recordLikeRender(this, path);
  }
}

Rec.unifyChildren = function(
  recL: Rec<Address>,
  recR: Rec<Address>
): Rec<Unif> {
  return recL.mergeWith((l, r) => ({ l, r }), recR);
};

hashable(Rec);

// eslint-disable-next-line no-unused-vars
export class RecTy<A> extends ExMap {
  modality = 'value';

  proposeTypingRelation() {
    return RecTyRelation.value;
  }

  // TODO checking constructor

  slate(path: SlatePath): SlateVal {
    return recordLikeRender(this, path);
  }

  // TODO handlers
}

// eslint-disable-next-line no-unused-vars
RecTy.unifyChildren = function (l: RecTy<Address>, r: RecTy<Address>): RecTy<Unif> {
  // TODO recordLikeUnify
  throw new Error('TODO RecTy.unifyChildren');
};

hashable(RecTy);

// Just accessing a field on a record: looks somthing like:
// `{ a: 1 }.a`
export class FieldAccess extends Record({ name: null }) {
  modality = 'computation';

  slate(path: SlatePath): SlateVal {
    const name = expand(this.name);
    // const rec = expand(this.rec);

    // TODO: we might want some way for this to be a block if the record is
    return new Inline({
      type: 'span',
      nodes: List([
        // rec.slate(path.concat(0)),
        mkText('.', mkStructuralKey(path, 0)),
        mkText(name, mkStructuralKey(path, 1)),
      ]),
      key: mkAddrKey(this.hash, path),
    });
  }

  normalize(arg: Address): Address {
    const name: Uninterpreted<Address> = expand(this.name);
    const rec: Rec<Address> = expand(arg);

    return rec.get(name.str);
  }

  proposeTypingRelation() {
  }
}

hashable(FieldAccess);

// TODO does this even make sense for computations?
FieldAccess.unifyChildren = function (
  l: FieldAccess<Address>,
  r: FieldAccess<Address>
): ?FieldAccess<Unif> {

}

export class Destructure extends Record({ body: null }){
  slate(path: SlatePath): SlateVal {
  }

  normalize(arg: Address): Address {
    const body: Term<Address> = expand(this.body);
    const rec: Rec<Address> = expand(arg);

    return close(body, rec);
  }
}
