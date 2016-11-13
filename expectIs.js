// @flow
/* eslint-env jest */

import { is } from 'immutable';

export default function expectIs(x: any, y: any): void {
  expect(is(x, y)).toBeTruthy();
}
