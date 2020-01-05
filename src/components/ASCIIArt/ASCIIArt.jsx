import React from 'react';
import PropTypes from 'prop-types';
import classNames from 'classnames';

const ASCIIArt = (props) => (
    <div className={classNames('ASCIIArt', props.className)}>
        {props.children}
    </div>
);


ASCIIArt.propTypes = {
    className: PropTypes.string,
    children: PropTypes.node
};

export default ASCIIArt;
