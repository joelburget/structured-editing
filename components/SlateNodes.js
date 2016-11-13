// @flow
import React from 'react';

type Props = {
  children: React$Element<*>;
  attributes: any;
  node: {
    data: any;
  };
};

const styles = {
  // underline philosophy: https://css-tricks.com/styling-underlines-web/
  conflict: {
    borderBottom: '2px solid rgba(255, 0, 0, 0.2)',
    display: 'inline-block',
    lineHeight: '0.85',
  },
  cell: { padding: 10, border: '2px solid #ddd' },
  headerCell: { borderBottom: '2px solid #ccc' },
  nonEditable: { backgroundColor: '#f7f7f7' },
  closeParen: {
    color: '#ddd',

    // this is an attempt to keep the cursor black with shadow text:
    // color: 'black',  /* change [input cursor color] by this*/
    // textShadow: '0px 0px 0px #ddd', /* change [input font] by this*/
    // webkitTextFillColor: 'transparent',
  },
};

// TODO figure out how to add onMouseEnter / onMouseLeave (is there like a
// Hover HOC?) to hint there are resolution controls
function conflict({ attributes, children }: Props): React.Element<*> {
  return (
    <span {...attributes} style={styles.conflict}>
      {children}
    </span>
  );
}

function table({ attributes, children }: Props): React.Element<*> {
  return (
    <table style={{ borderCollapse: 'collapse' }} {...attributes}>
      <tbody>{children}</tbody>
    </table>
  );
}

function tableRow({ attributes, children }: Props): React.Element<*> {
  return <tr {...attributes}>{children}</tr>;
}

function tableCell({ attributes, children, node }: Props): React.Element<*> {
  const contentEditable = node.data.get('contentEditable');
  const style = contentEditable
    ? styles.cell
    : { ...styles.cell, ...styles.nonEditable };

  // Only use the contentEditable attribute if we're marking this node as *not*
  // editable, since react warns (spuriously) on `contentEditable={true}`.
  const editableAttr = !contentEditable ? { contentEditable } : {};

  return (
    <td {...editableAttr} style={style} {...attributes}>
      {children}
    </td>
  );
}

function tableHeader({ attributes, children }: Props): React.Element<*> {
  return (
    <th
      contentEditable={false}
      style={{ ...styles.cell, ...styles.nonEditable, ...styles.headerCell }}
      {...attributes}
    >
      {children}
    </th>
  );
}

function closeParen({ attributes, children }: Props): React.Element<*> {
  return <span style={styles.closeParen} {...attributes}>{children}</span>;
}

// schema
export default {
  nodes: { conflict, table, tableRow, tableCell, tableHeader, closeParen },
};
