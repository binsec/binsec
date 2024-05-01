include Common

let bitwuzla_c : (module F) option = Bitwuzla_c_binding.factory
let bitwuzla_cxx : (module F) option = Bitwuzla_cxx_binding.factory
