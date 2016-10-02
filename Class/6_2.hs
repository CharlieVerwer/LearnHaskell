main = do
		x <- (++) <$> getLine <*> getLine
		putStrLn x

		-- line' <- fmap reverse getLine
		-- putStrLn line'