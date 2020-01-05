import React from 'react';
import PropTypes from 'prop-types';

const Terminal = (props) => (
    <div className = {`Terminal ${props.className}`}>
        {props.children}
    </div>
);

Terminal.propTypes = {
    className: PropTypes.string,
    children: PropTypes.node
};

export default Terminal;
