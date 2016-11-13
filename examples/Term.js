// @flow

import { set as setAddr } from '../Address';
import { Conflict, Ty, Lisp, Ref } from '../forms/bootstrap';
import { names } from '../name-registry';
import { Int, IntTy, Addition } from '../forms/Int';
import { Bool, BoolTy, Or } from '../forms/Bool';
import Uninterpreted from '../forms/Uninterpreted';
import { Workspace } from '../forms/Workspace';

const addNameAddr = names.add.addr;
const colonNameAddr = names[':'].addr;

const one = new Int({ i: 1 });
export const oneAddr = setAddr(one);

const two = new Int({ i: 2 });
export const twoAddr = setAddr(two);

const t = new Bool({ b: true });
export const trueAddr = setAddr(t);

const f = new Bool({ b: false });
export const falseAddr = setAddr(f);

const conflict = new Conflict({
  value: setAddr(Ty.value),
  outerTy: setAddr(IntTy.value),
  innerTy: setAddr(Ty.value),
});
const conflictAddr = setAddr(conflict);

export const onePlusTwo = new Addition({ l: oneAddr, r: twoAddr });
export const onePlusTwoAddr = setAddr(onePlusTwo);

const tOrF = new Or({ l: trueAddr, r: falseAddr });
export const tOrFAddr = setAddr(tOrF);

export const emptyLisp = new Lisp([addNameAddr]);
export const emptyLispAddr = setAddr(emptyLisp);

export const addLisp = new Lisp([addNameAddr, oneAddr, twoAddr]);
export const addLispAddr = setAddr(addLisp);

const conflictExample = new Lisp([addNameAddr, twoAddr, conflictAddr]);
export const conflictExampleAddr = setAddr(conflictExample);

const annotationExample = new Lisp([colonNameAddr, twoAddr, setAddr(IntTy.value)]);
export const annotationExampleAddr = setAddr(annotationExample);

const badTypeExample = new Lisp([colonNameAddr, oneAddr, setAddr(Ty.value)]);
const badTypeExampleAddr = setAddr(badTypeExample);

const oneNameAddr = setAddr(new Uninterpreted({ str: 'one' }));
const oneHashAddr = setAddr(new Uninterpreted({ str: oneAddr }));
const namedRefAddr = setAddr(new Ref({ name: oneNameAddr, refHash: oneHashAddr }));
const unnamedRefAddr = setAddr(new Ref({ refHash: oneHashAddr }));

const exampleExample = new Workspace([
  conflictAddr,
  oneAddr,
  annotationExampleAddr,
  badTypeExampleAddr,
  onePlusTwoAddr,
  tOrFAddr,
  namedRefAddr,
  unnamedRefAddr,
  trueAddr,
]);
export const exampleExampleAddr = setAddr(exampleExample);

// 1 + (true || 2)
const innerConflict = setAddr(
  new Conflict({
    value: twoAddr,
    outerTy: setAddr(BoolTy.value),
    innerTy: setAddr(IntTy.value),
  })
);
const between = setAddr(
  new Or({ l: trueAddr, r: innerConflict })
);
const outerConflict = setAddr(
  new Conflict({
    value: between,
    innerTy: setAddr(BoolTy.value),
    outerTy: setAddr(IntTy.value),
  })
);
const outside = setAddr(
  new Addition({ l: oneAddr, r: outerConflict })
);
export const doubleConflictAddr = outside;
