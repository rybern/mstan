// When you go to merge at model, you collide h:i4 with h:i2,h2:i3, causing h:i4,h2:i3
model {
  a();
  b();
}
module "b" b() {
  parameters {
    b;
  }
  b;
  h();
}
module "a" a() {
  parameters {
    a;
  }
  a;
  h();
}
module "i2" h() {
  parameters {
    i2;
  }
  i2;
  h2();
}
module "i3" h2() {
  parameters {
    i3;
  }
  i3;
}
module "i4" h() {
  parameters {
    i4;
  }
  i4;
}
