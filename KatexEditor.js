// @flow
import React from 'react';
import {katexIsTemplated} from './purescript/output/Katex';

import {
  mkDataDecorator,
  StructureDisplay,
} from './StructuredEditor';

const components = {
  internal: ({children}) => {
    // assert React.Children.count(children) === 2
    // const [l, r] = children;
    return <span>{children}</span>;
  },
  leaf: ({children}) => {
    return <span style={styles.leaf}>{children}</span>
  },
  hole: ({children}) => (
    // TODO
    // {React.Children.only(children)}
    <span style={styles.hole}>{children}</span>
  ),
  conflict: ({children}) => (
      <span style={styles.conflict}>{children}</span>
  ),
};

const DataDecorator = mkDataDecorator(components);

export function KatexDisplay({opaqueSyntax}) {
  return (
    <StructureDisplay
      opaqueSyntax={opaqueSyntax}
      templatedInstance={katexIsTemplated}
      DataDecorator={DataDecorator}
    />
  );
}

const styles = {
  leaf: {
    backgroundColor: 'rgba(0, 0, 255, 0.09)',
  },
  hole: {
    backgroundColor: 'rgba(255, 177, 0, 0.25)',
  },
  conflict: {
    backgroundColor: 'rgba(255, 0, 0, 0.09)',
  },
};
