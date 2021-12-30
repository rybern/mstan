model {
  A() + B();
}

module "a1" A() {
  return 1;
}
module "a2" A() {
  return 2;
}
module "b3" B() {
  return 3;
}
module "b4" B() {
  return 4;
}
