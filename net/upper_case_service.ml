open Net.Server
open Net.UpperCase

let main = go (uncurry upper_case_service)

