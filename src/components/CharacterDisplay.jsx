import React from 'react';
import { motion } from 'framer-motion';
import { Box, Typography, Paper } from '@mui/material';

function CharacterDisplay({ solution, description, isLoading, questionCount }) {
  return (
    <motion.div
      layout
      className="mb-8 min-h-[200px] flex items-center justify-center"
    >
      {solution ? (
        <motion.div
          initial={{ scale: 0 }}
          animate={{ scale: 1 }}
          className="text-center w-full"
        >
          <Paper elevation={3} className="p-6 bg-purple-50">
            <Typography variant="h4" className="text-purple-600 mb-4">
              ¡Es {solution}!
            </Typography>
            <Typography variant="body1" className="text-gray-700 italic">
              {description}
            </Typography>
          </Paper>
        </motion.div>
      ) : (
        <Typography variant="body1" className="text-gray-600 italic">
          {isLoading ? 'Pensando...' : questionCount === 0 ? 'Piensa en un personaje...' : 'Analizando tus respuestas...'}
        </Typography>
      )}
    </motion.div>
  );
}

export default CharacterDisplay;