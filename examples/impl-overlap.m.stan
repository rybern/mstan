model { S1(); S2(); }

module "A" S1() {
  S3();
}

module "B" S2() {
  S3();
}

module "C" S3() {
}

module "D" S3() {
}
