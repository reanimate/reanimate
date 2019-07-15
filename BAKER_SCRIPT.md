# Baker's Algorithm

A computer's memory is finite and will quickly run out if it is not reused. Manually marking objects for reuse is error prone
and automatic memory management techniques have become the default in high level languages.

Copying garbage collector divide available memory into two segments and marks one of them for new allocations.
No memory is reused until the segment is full. When this happens, reachable objects are copied to the opposite segment. Anything
left must be unreachable and the memory in the segment can be reused. 
