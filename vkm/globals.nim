type ESystem* = enum
  LeftHand
  RightHand

const System* =
  when defined(LeftHand): LeftHand
  elif defined(RightHand): RightHand
  else: RightHand