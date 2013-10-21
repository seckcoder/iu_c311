(library
    (elegant-weapons compat)
  (export add1 sub1 atom? syntax-error make-parameter parameterize
    last-pair make-list void andmap datum->syntax-object syntax-object->datum)
  (import (only (chezscheme) add1 sub1 atom? syntax-error make-parameter
                parameterize last-pair make-list void andmap datum->syntax-object syntax-object->datum)
          (rnrs))
  )
