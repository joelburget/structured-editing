// @flow
import { List as ExList } from 'extendable-immutable';

import { hashable } from '../decorators';

import type { Address } from '../Address';
import type { Term, Meta, Unif, UnificationResult } from '../types';
import type { SlatePath, SlateVal } from '../slateHelpers';

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
