import React from 'react';
import PropTypes from 'prop-types';
import classNames from 'classnames';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faTimesCircle, faMinusCircle } from '@fortawesome/free-solid-svg-icons';

const Terminal = (props) => (
    <div className = {classNames('terminal', props.className)}>
        <div className = "menu-bar">
            <FontAwesomeIcon 
                className = "select-icon exit" 
                icon = { faTimesCircle }
                onClick = { props.onExit}
            />
            <FontAwesomeIcon 
                className = "select-icon minimize" 
                icon = { faMinusCircle }
                onClick = { props.onMinimize }
            />
            <span className="window-name">
                {props.windowName}
            </span>
        </div>
        <div className = "window">
            {props.children}
        </div>
    </div>
);

Terminal.propTypes = {
    className: PropTypes.string,
    children: PropTypes.node,
    onExit: PropTypes.func,
    onMinimize: PropTypes.func,
    windowName: PropTypes.string
};

export default Terminal;
