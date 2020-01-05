import React from 'react';
import PropTypes from 'prop-types';

const ASCIIArt = (props) => (
    <div className={`ASCIIArt ${props.className}`}>
        {props.children.map(x => <p className="ascii-line"> {x} </p>)}
    </div>
);


ASCIIArt.propTypes = {
    className: PropTypes.string,
    children: PropTypes.node
};

export default ASCIIArt;
