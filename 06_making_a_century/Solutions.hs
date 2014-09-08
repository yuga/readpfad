
solutions = filter (good . value) . foldr expand' []

