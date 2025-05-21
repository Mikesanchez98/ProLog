import React from 'react';
import { motion } from 'framer-motion';
import { Typography, Box } from '@mui/material';

function QuestionArea({ currentQuestion, isLoading, remainingCharacters }) {
  const formatQuestion = (attr) => {
    if (!attr) return '';
    const attrText = attr.replace(/_/g, ' ');
    return `¿Tu personaje ${attrText}?`;
  };

  return (
    <motion.div
      layout
      className="mb-8 min-h-[100px] flex flex-col items-center justify-center"
    >
      <Typography variant="h5" className="text-center mb-4">
        {formatQuestion(currentQuestion)}
      </Typography>
      
      <Typography variant="body2" className="text-gray-600">
        Quedan {remainingCharacters} personajes posibles
      </Typography>
    </motion.div>
  );
}

export default QuestionArea;