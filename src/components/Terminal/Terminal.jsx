import React from 'react';
import PropTypes from 'prop-types';
import classNames from 'classnames';

const Terminal = (props) => (
    <div className = {classNames('Terminal', props.className)}>
        {props.children}
    </div>
);

Terminal.propTypes = {
    className: PropTypes.string,
    children: PropTypes.node
};

export default Terminal;
