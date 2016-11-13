/* eslint-env jest */

import { typecheck, unify, BadRelation } from '../../unify';
import { Ty } from '../bootstrap';
import { Int, IntTy } from '../Int';
import { set as setAddr } from '../../Address';
import { addLispAddr, oneAddr, twoAddr, onePlusTwoAddr }
  from '../../examples/Term';

describe('basic unification', () => {
  test('Ty ~ Ty', () => {
    const { type, value } = unify(Ty.addr, Ty.addr);
    expect(type).toBe('OuterSuccess');
    expect(value).toBe(Ty.value);
  });

  test('Ty ~> Ty', () => {
    const { type, value } = unify(Ty.addr, null);
    expect(type).toBe('OuterSuccess');
    expect(value).toBe(Ty.value);
  });

  test('Ty <~ Ty', () => {
    const { type, value } = unify(null, Ty.addr);
    expect(type).toBe('OuterSuccess');
    expect(value).toBe(Ty.value);
  });

  test('IntTy ~ IntTy', () => {
    const { type, value } = unify(IntTy.addr, IntTy.addr);
    expect(type).toBe('OuterSuccess');
    expect(value).toBe(IntTy.value);
  });
});

describe('basic typechecking', () => {
  test('Ty :~ Ty', () => {
    const [tm, ty] = typecheck(Ty.value, Ty.value, Ty.value.proposeTypingRelation());

    expect(tm).toBe(Ty.value);
    expect(ty).toBe(Ty.value);
  });

  test('Ty :~> Ty', () => {
    const [tm, ty] = typecheck(Ty.value, null, Ty.value.proposeTypingRelation());

    expect(tm).toBe(Ty.value);
    expect(ty).toBe(Ty.value);
  });
});

/*
describe('lisp synthesis', () => {
  test('Lisp becomes an addition when pushed', () => {
    const {type, values: [tm, ty]} = typecheck(addLispAddr, IntTy.addr);

    // TODO
    // expect(tm).toBe(Addition);
    expect(ty).toBe(IntTy.value);
  });
});
*/

describe('finding problems', () => {
  test('1 :/~ Ty', () => {
    const one = new Int({ i: 1 });
    const val = typecheck(one, Ty.value, one.proposeTypingRelation());

    expect(val).toBeInstanceOf(BadRelation);
    const { tm, ty} = val;
    expect(tm).toBe(one);
    expect(ty).toBe(Ty.value);
  });
});

describe('stitching problems together', () => {
  // for this i think a row type example would be most natural, though a
  // function might also be interesting
});
