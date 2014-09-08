package org.estewei.tseq

package object data {

  type FastTCQueue[C[_, _], A, B] = CTQueue[RTQueue, C, A, B]

}
