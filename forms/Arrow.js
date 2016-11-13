// @flow

import { List, Record } from 'immutable';
import { Inline } from 'slate';

import { hashable } from '../decorators';
import { expand } from '../Address';
import { mkStructuralKey, mkText } from '../slateHelpers';

import type { Address } from '../Address';
import type { SlatePath, SlateVal } from '../slateHelpers';

type Fields = {
  domain: Address;
  codomain: Address;
};

export default class Arrow<A> extends Record({ domain: null, codomain: null }) {
  modality = 'value';

  slate(path: SlatePath): SlateVal {
    const { domain, codomain }: Fields = this;
    let i: number = 0;

    return new Inline({
      type: 'span',
      nodes: List([
        expand(domain).slate(path.concat(i++)),
        // TODO: improve the look of this arrow
        mkText(' → ', mkStructuralKey(path, i++)),
        expand(codomain).slate(path.concat(i++)),
      ]),
      key: mkStructuralKey(path, i++),
    });
  }
}

hashable(Arrow);
