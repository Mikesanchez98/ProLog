import React from 'react';
import { motion } from 'framer-motion';
import { Button, Box } from '@mui/material';

function GameControls({ onYes, onNo, onReset, disabled }) {
  const buttonVariants = {
    hover: { scale: 1.05 },
    tap: { scale: 0.95 },
  };

  return (
    <Box className="flex justify-center gap-4">
      <motion.div variants={buttonVariants} whileHover="hover" whileTap="tap">
        <Button
          variant="contained"
          color="success"
          onClick={onYes}
          disabled={disabled}
          className="px-8 py-2"
        >
          Sí
        </Button>
      </motion.div>

      <motion.div variants={buttonVariants} whileHover="hover" whileTap="tap">
        <Button
          variant="contained"
          color="error"
          onClick={onNo}
          disabled={disabled}
          className="px-8 py-2"
        >
          No
        </Button>
      </motion.div>

      <motion.div variants={buttonVariants} whileHover="hover" whileTap="tap">
        <Button
          variant="outlined"
          color="primary"
          onClick={onReset}
          className="px-8 py-2"
        >
          Reiniciar
        </Button>
      </motion.div>
    </Box>
  );
}

export default GameControls;