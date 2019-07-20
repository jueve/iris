class OpenBus {
  private var nmiStatus = false
  private var cpuStallStatus = false

  def setNmi(): Unit = nmiStatus = true
  def clearNmi(): Unit = nmiStatus = false
  def nmiCondition: Boolean = nmiStatus

  def setCpuStall(): Unit = cpuStallStatus = true
  def clearCpuStall(): Unit = cpuStallStatus = false
  def cpuStallCondition: Boolean = cpuStallStatus
}
