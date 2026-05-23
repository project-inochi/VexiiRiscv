// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.riscv

import spinal.core._

object Rvk {
  import IntRegFile._

  /* Both AES32ES* and AES32DS* */
  def AES32EDS  = TypeR(M"--10--1----------000-----0110011")

  def AES64EDS  = TypeR(M"0011--1----------000-----0110011")
  // def AES64ES   = TypeR(M"0011001----------000-----0110011")
  // def AES64ESM  = TypeR(M"0011011----------000-----0110011")
  // def AES64DS   = TypeR(M"0011101----------000-----0110011")
  // def AES64DSM  = TypeR(M"0011111----------000-----0110011")
  def AES64KS1I = TypeR(M"00110001---------001-----0010011")
  def AES64KS2  = TypeR(M"0111111----------000-----0110011")
  def AES64IM   = TypeI(M"001100000000-----001-----0010011")
}
