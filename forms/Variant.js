// @flow

import { Set as ExSet, Map as ExMap } from 'extendable-immutable';
import { List, Record } from 'immutable';
import { Inline } from 'slate';

import { Ty } from './bootstrap';
import { tableRow, tableCell, mkStructuralKey, mkAddrKey, mkText }
  from '../slateHelpers';
import { hashable, mapSubtermsIsMap } from '../decorators';
import { expand } from '../Address';

import type { Address } from '../Address';
import type { SlatePath, SlateVal } from '../slateHelpers';
import type { Term, Unif } from '../types';
import type Uninterpreted from './Uninterpreted';

export class NominalVariantRelation {
  stitch(
    tm: NominalVariant<Address>,
    ty: NominalVariantTy<Address>
  ): [NominalVariant<Unif>, NominalVariantTy<Unif>] {
    // TODO stitching when one is null, part of which is implementing
    // information flow from ty to tm
    const name: Uninterpreted<Address> = tm.get('tag');
    return [tm, ty.add(name)];
  }

  accepts(tm: Term<Address>, ty: Term<Address>) {
    return tm instanceof NominalVariant && ty instanceof NominalVariantTy;
  }
}

NominalVariantRelation.value = new NominalVariantRelation;

export class NominalVariantTyRelation {
  stitch(
    tm: NominalVariantTy<Address>,
    ty: Ty<Address> // eslint-disable-line no-unused-vars
  ): [NominalVariantTy<Address>, Ty<Address>] {
    return [tm, ty];
  }

  accepts(tm: Term<Address>, ty: Term<Address>) {
    return tm instanceof NominalVariantTy && ty instanceof Ty;
  }
}

NominalVariantTyRelation.value = new NominalVariantTyRelation;

export class NominalVariant<A> extends Record({ tag: null }) {
  slate(path: SlatePath): SlateVal {
    return mkText(this.tag, mkStructuralKey(path, 0));
  }
}

NominalVariant.unifyChildren = function (
  l: NominalVariant<Address>,
  r: NominalVariant<Address>
): ?NominalVariant<Unif> {
  return l === r ? l : new NominalVariant({ tag: { l: l.tag, r: r.tag } });
}

hashable(NominalVariant);
mapSubtermsIsMap(NominalVariant);

export class NominalVariantTy<A> extends ExSet {
  proposeTypingRelation() {
    return NominalVariantRelation.value;
  }

  slate(path: SlatePath): SlateVal {
    let i = 0;
    const tags = [];
    this.forEach(childAddr => {
      tags.push(
        expand(childAddr).slate(path.concat(i++)),
        mkText('|', mkStructuralKey(path, i++))
      );
    });
    tags.unshift(mkText('⟨', mkStructuralKey(path, i++)));
    tags.splice(-1, 1, mkText('⟩', mkStructuralKey(path, i++)));

    return new Inline({
      type: 'span',
      nodes: List(tags),
      key: mkStructuralKey(path, i++),
    })
  }
}

hashable(NominalVariantTy);
mapSubtermsIsMap(NominalVariantTy);

NominalVariantTy.unifyChildren = function (
  l: NominalVariantTy<Address>,
  r: NominalVariantTy<Address>
): NominalVariantTy<Unif> {

  return l.intersect(r); // intersect the tags
};


type VariantCaseFields = {

}

export class VariantCase extends ExMap {
  modality = 'computation';

  slate(path: SlatePath): SlateVal {
    return new Inline({
      type: 'span',
      nodes: List(mkText('TODO: variant case', mkStructuralKey(path, 0))),
      key: mkStructuralKey(path, 1),
    });
  }

  normalize(arg: Address): Address {
    // TODO this is weak -- have a tag type
    const tag: Uninterpreted<Address> = expand(arg);

    // TODO what if tag doesn't exist?
    return this.get(tag.str);
  }

  proposeTypingRelation() {}
}

mapSubtermsIsMap(NominalVariantTy);
