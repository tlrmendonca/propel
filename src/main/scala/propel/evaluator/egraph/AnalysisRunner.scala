package propel.evaluator.egraph

import propel.evaluator.egraph.{EClass, ENode, Language}
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{ArrayBuffer, Seq as MutableSeq}

/** 
 * A tool to allow the use of many analysis' simultaneously.
 * @note Naive implementation. In the future, each analysis can influence the next.
 * @note This should in theory extend [[Analysis]], but for simplicity's sake it doesn't.
 * **Code Duplication warning**
*/
class AnalysisRunner {
  
    private val analysisList: ArrayBuffer[Analysis] = ArrayBuffer.empty;

    def add(analysis: Analysis): Unit = {
        analysisList += analysis;
    }

    def getAnalysisList(): Seq[Analysis] = {
        return analysisList.toSeq;
    }

    /**
      * Runs *make* for each analysis.
      * 
      * @param egraph the specified [[Egraph]].
      * @param enode the specified [[ENode]].
      * 
      */
    def make[G](egraph: G, enode: ENode)(using EGraphOps[G]): Unit = {
        analysisList.foreach(analysis => 
            val data = analysis.make(egraph, enode)
            analysis.setData(egraph.find(EClass(enode)).id, data)
        )
    }

    /**
      * Runs *merge* for each analysis.
      *
      * @param data1 the data of the first EClass.
      * @param data2 the data of the second EClass, to be merged.
      */
    def merge(id1: EClass.Id, id2: EClass.Id): Unit = {
        analysisList.foreach(analysis =>
            val data1 = analysis.getData(id1).get
            val data2 = analysis.getData(id2).get

            val data = analysis.merge(data1, data2)
            
            // Note: setting data on both garantees the unioned class will 
            // have the final data (while the other is not of interest anymore) 
            analysis.setData(id1, data) 
            analysis.setData(id2, data) 
        )
    }

    /**
      * Runs *modify* for each analysis.
      *
      * @param egraph the specified [[Egraph]].
      * @param id an [[EClass]]'s Id.
      * 
      * @note This function must be idempotent, i.e. modify(modify()) = 
      * modify(). Usually adds an [[ENode]] to the given [[EClass]].
      */
    def modify[G](egraph: G, id: EClass.Id)(using EGraphOps[G]): Unit = {
        analysisList.foreach(analysis => analysis.modify(egraph, id))
    }
}
