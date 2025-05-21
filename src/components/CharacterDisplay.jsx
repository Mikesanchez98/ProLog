import React from 'react';
import { motion } from 'framer-motion';
import { Box, Typography } from '@mui/material';

function CharacterDisplay({ solution, isLoading }) {
  return (
    <motion.div
      layout
      className="mb-8 h-64 flex items-center justify-center rounded-lg bg-gray-100"
    >
      {solution ? (
        <motion.div
          initial={{ scale: 0 }}
          animate={{ scale: 1 }}
          className="text-center"
        >
          <div className="w-48 h-48 bg-gray-300 rounded-full mb-4 mx-auto">
            {/* Placeholder for character image */}
          </div>
          <Typography variant="h4" className="text-purple-600">
            ¡Es {solution}!
          </Typography>
        </motion.div>
      ) : (
        <Typography variant="body1" className="text-gray-600 italic">
          {isLoading ? 'Pensando...' : 'Piensa en un personaje...'}
        </Typography>
      )}
    </motion.div>
  );
}

export default CharacterDisplay;