package propel.evaluator.egraph

import propel.evaluator.egraph.{EClass, ENode, Language}
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{ArrayBuffer, Seq as MutableSeq, Map as MutableMap, Set as MutableSet}

/** 
 * A tool to allow the use of many analysis' simultaneously.
 * @note Naive implementation. In the future, each analysis can influence the next.
 * @note This should in theory extend [[Analysis]], but for simplicity's sake it doesn't.
 * **Code Duplication warning**
*/
class AnalysisRunner {
  
    private val analysisList: ArrayBuffer[Analysis] = ArrayBuffer.empty;

    def add(analysis: Analysis): Unit = {
        // this call must be recursive
        analysis.dependencies.foreach(dep => add(dep));
        if !analysisList.contains(analysis) then
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
         * @note This function deletes data2 after merging.
         */
    def merge(id1: EClass.Id, id2: EClass.Id): Unit = {
        analysisList.foreach(analysis =>
            val data1 = analysis.getData(id1).get
            val data2 = analysis.getData(id2).get

            val data = analysis.merge(data1, data2)
            
            analysis.setData(id1, data) 
            analysis.deleteData(id2)
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

    /**
      * Runs repair for each analysis.
      * 
      * @param egraph the specified [[Egraph]].
      * @param uses the uses of the [[ENode]].
      * @param worklist the worklist of the [[EGraph]], i.e. nodes to run repair in the future.
      */
    def repair[G](egraph: G, uses: MutableMap[ENode, EClass], worklist: MutableSet[EClass.Id])(using EGraphOps[G]): Unit = {
      uses.foreach((x, xcStale) =>
        val x0 = egraph.canonicalize(x)
        (x0, egraph.enodes.get(x0).map(id => egraph.getEClassFromId(id))) match
          case (_, Some(xcStale0)) =>
            analysisList.foreach(analysis =>
              val data = analysis.getData(xcStale0.id).get //FIXME: Unsafe
              val data2 = analysis.make(egraph, x)
              val newData = analysis.merge(data, data2)

              if newData != data then
                analysis.setData(xcStale0.id, newData)
                worklist.add(xcStale0.id) // This is only fine inside the forEach because it is a **Set**
            )
          case _ => throw new java.lang.Exception("Something went wrong in analysis rebuilding")
        )
    }
}
