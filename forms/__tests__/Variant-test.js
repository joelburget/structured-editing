/* eslint-env jest */

import { set as setAddr, expand, debugExpand } from '../../Address';
import { typecheck, unify, BadRelation } from '../../unify';
import { NominalVariant, NominalVariantTy, NominalVariantRelation }
  from '../Variant';
import { Int, IntTy, IntRelation } from '../Int';
import { addLispAddr, oneAddr, twoAddr, onePlusTwoAddr }
  from '../../examples/Term';
import Uninterpreted from '../Uninterpreted';
import expectIs from '../../expectIs';

const aAddr = setAddr(new Uninterpreted({ str: 'a' }))
const bAddr = setAddr(new Uninterpreted({ str: 'b' }))
const cAddr = setAddr(new Uninterpreted({ str: 'c' }))
const abVarTy = new NominalVariantTy([aAddr, bAddr]);
const bVarTy = new NominalVariantTy([bAddr]);
const bcVarTy = new NominalVariantTy([bAddr, cAddr]);
const aVar = new NominalVariant({ tag: aAddr });
const bVar = new NominalVariant({ tag: bAddr });

describe('unification', () => {
  test('a ~ a', () => {
    const { type, value } = unify(setAddr(aVar), setAddr(aVar));

    expect(type).toBe('OuterSuccess');
    expectIs(value, aVar);
  });

  test('a /~ b', () => {
    const { type, value } = unify(setAddr(aVar), setAddr(bVar));

    expect(type).toBe('OuterSuccess');
    expect(value.get('tag')).toEqual({
      type: 'Failure',
      value: {
        l: aAddr,
        r: bAddr,
      },
    });
  });

  test('<a | b> ~ <a | b>', () => {
    const { type, value } = unify(setAddr(abVarTy), setAddr(abVarTy));

    expect(type).toBe('OuterSuccess');
    expectIs(value, abVarTy);
  });

  test('<a | b> ~ <b | c>', () => {
    const { type, value } = unify(setAddr(abVarTy), setAddr(bcVarTy));
    expect(type).toBe('OuterSuccess');
  });
});

describe('typechecking', () => {
  test('a :~ <a | b>', () => {
    const [tm, ty] = typecheck(aVar, abVarTy, NominalVariantRelation.value);
    expectIs(tm, aVar);
    expectIs(ty, abVarTy);
  });

  // XXX I'm really not sure this is right -- should unification add to the
  // variant? Is it closed?
  test('a :~ <b>', () => {
    const [tm, ty] = typecheck(aVar, bVarTy, NominalVariantRelation.value);
    expectIs(tm, aVar);
    expectIs(ty, abVarTy);
  });
})
