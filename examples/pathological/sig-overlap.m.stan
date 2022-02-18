model { S1(); }

module "A" S1() {
  S2();
}

module "B" S1() {
  S2();
}

module "C" S2() {
}

module "D" S2() {
}
