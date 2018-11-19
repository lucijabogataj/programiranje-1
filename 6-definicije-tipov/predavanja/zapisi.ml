type kompleksno = {re : float; im : float}

let nic = {re = 0.0; im = 0.0}

let ena = {nic with re = 1.0}

let_add_one_to_im x = {re = x.re; im = x.im +. 1.}
