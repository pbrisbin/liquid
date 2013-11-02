# Liquid Haskell

Haskell implementation of Liquid templating by Shopify.

# Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.Liquid
import Text.Liquid.Context

liquid [("name", CVar "Pat")] "Hi {{name}}!"
-- => "Hi Pat!"
```

## Specs

```sh
for s in spec/*.hs; do
  runhaskell "$s"
done
```
