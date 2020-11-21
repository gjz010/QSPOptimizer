import cirq
def optimize(c):
        cirq.ConvertToCzAndSingleGates().optimize_circuit(circuit=c)
        cirq.MergeSingleQubitGates().optimize_circuit(circuit=c)
        cirq.EjectZ().optimize_circuit(circuit=c)
        cirq.EjectPhasedPaulis().optimize_circuit(circuit=c)
        cirq.MergeSingleQubitGates().optimize_circuit(circuit=c)
        cirq.DropNegligible().optimize_circuit(circuit=c)
        cirq.DropEmptyMoments().optimize_circuit(circuit=c)
        c2=cirq.Circuit()
        for g in c:
                for g2 in g:
        #               print(g2)
                        c2.append(g2,strategy=cirq.InsertStrategy.EARLIEST)
        return c2
