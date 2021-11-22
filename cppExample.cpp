

  if (!I0 || !I1 || I0->getOpcode() != I1->getOpcode())
    return nullptr;

  bool isMpy = false;
  if (I0->getOpcode() == Instruction::FMul)
    isMpy = true;
  else if (I0->getOpcode() != Instruction::FDiv)
    return nullptr;

  Value *Opnd0_0 = I0->getOperand(0);
  Value *Opnd0_1 = I0->getOperand(1);
  Value *Opnd1_0 = I1->getOperand(0);
  Value *Opnd1_1 = I1->getOperand(1);

  //  Input Instr I       Factor   AddSub0  AddSub1
  //  ----------------------------------------------
  // (x*y) +/- (x*z)        x        y         z
  // (y/x) +/- (z/x)        x        y         z
  //
  Value *Factor = nullptr;
  Value *AddSub0 = nullptr, *AddSub1 = nullptr;

  if (isMpy) {
    if (Opnd0_0 == Opnd1_0 || Opnd0_0 == Opnd1_1)
      Factor = Opnd0_0;
    else if (Opnd0_1 == Opnd1_0 || Opnd0_1 == Opnd1_1)
      Factor = Opnd0_1;

    if (Factor) {
      AddSub0 = (Factor == Opnd0_0) ? Opnd0_1 : Opnd0_0;
      AddSub1 = (Factor == Opnd1_0) ? Opnd1_1 : Opnd1_0;
    }
  } else if (Opnd0_1 == Opnd1_1) {
    Factor = Opnd0_1;
    AddSub0 = Opnd0_0;
    AddSub1 = Opnd1_0;
  }

  if (!Factor)
    return nullptr;

  FastMathFlags Flags;
  Flags.setUnsafeAlgebra();
  if (I0) Flags &= I->getFastMathFlags();
  if (I1) Flags &= I->getFastMathFlags();

  // Create expression "NewAddSub = AddSub0 +/- AddsSub1"
  Value *NewAddSub = (I->getOpcode() == Instruction::FAdd) ?
                      createFAdd(AddSub0, AddSub1) :
                      createFSub(AddSub0, AddSub1);
