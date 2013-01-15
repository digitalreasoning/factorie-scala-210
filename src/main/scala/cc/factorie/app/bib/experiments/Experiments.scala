package cc.factorie.app.bib.experiments
import cc.factorie.app.bib._
import java.util.Date
import java.io._
import collection.mutable.{ArrayBuffer,HashSet,HashMap}
import java.text.DateFormat
import cc.factorie.util.{CmdOption, DefaultCmdOptions}
import com.mongodb.Mongo
import cc.factorie._
import app.nlp.coref._
import io.Source

class DebugDiffList extends DiffList{
  override def scoreAndUndo(model:Model): Double = {
    for(family <- model.families){
      family match{
        case f:DebugableTemplate => f.debugOn
        case _ => {}
      }
    }
    if (this.length == 0) return 0.0  // short-cut the simple case
    println("=====DEBUGGING MODEL SCORE=====")
    println("----NEXT WORLD----")
    var s = model.currentScore(this)
    println("  next: "+ s)
    //log(Log.DEBUG)("DiffList scoreAndUndo  pre-undo score=" + s)
    this.undo
    // We need to re-calculate the Factors list because the structure may have changed
    println("----CURRENT WORLD----")
    val s2 = model.currentScore(this)
    println("  current: "+s2)
    s -= s2
    println("TOTAL SCORE: "+s)
    for(family <- model.families){
      family match{
        case f:DebugableTemplate => f.debugOff
        case _ => {}
      }
    }
    //log(Log.DEBUG)("DiffList scoreAndUndo post-undo score=" + s)
    s
  }}
class AuthorSamplerWriter(model:Model, val initialDB:Seq[AuthorEntity], val evidenceBatches:Seq[Seq[AuthorEntity]], val initialDBNameOpt:Option[String]=None,val evidenceBatchNames:Option[Seq[String]]=None,var initialSteps:Int=0,stepsPerBatch:Int=10000,initInstructionsOpt:Option[Seq[Seq[()=>Unit]]]) extends AuthorSampler(model) with HumanEditDebugUtils{
  protected var pwOption:Option[PrintWriter]=None
  val labeledData = initialDB.filter(_.groundTruth != None)
  def snapshotInterval = 10000
  var batchCount = 0
  var mentionCount = labeledData.filter(_.isObserved).size
  var gtEntityCount = labeledData.filter((e:AuthorEntity) => {e.isObserved && e.groundTruth != None}).map(_.groundTruth.get).toSet.size
  var curScore:Double = 0.0
  var maxScore:Double = 0.0
  private var currentBatchName = "initial"
  if(initialDBNameOpt!=None)currentBatchName = initialDBNameOpt.get
  def processExperiment(pw:PrintWriter):Unit ={
    batchCount = 0
    pwOption=Some(pw)
    println("LABELED DATA SIZE "+labeledData.size)
    setEntities(initialDB)
    pw.println("time samples accepted f1 p r batch-count mentions entities batch-name score maxscore")
    //snapshot(totalTime,proposalCount,numAccepted,Evaluator.pairF1LabeledOnly(labeledData))
    println("Inferring initial database.")
    timeAndProcess(initialSteps)
    snapshot(totalTime,proposalCount,numAccepted,Evaluator.pairF1LabeledOnly(labeledData))
    println("About to process evidence stream.")
    for(evidenceBatch <- evidenceBatches){
      batchCount += 1
      evidenceBatch.foreach(this.addEntity(_))
      if(initInstructionsOpt!=None){
        println("Executing initialization instructions")
        for(initInstruction <- initInstructionsOpt.get.apply(batchCount-1)){
          initInstruction.apply()
        }
      }
      if(evidenceBatchNames != None)currentBatchName = evidenceBatchNames.get(batchCount-1)
      println("\n---Adding new evidence batch---")
      println("  Batch name: "+currentBatchName)
      println("  Mentions added: "+evidenceBatch.size)
      println("  Mentions added: ")
      evidenceBatch.map((e:AuthorEntity)=>EntityUtils.prettyPrintAuthor(e.entityRoot.asInstanceOf[AuthorEntity]))
      println("  Mentions added: "+evidenceBatch.map(_.id).toSeq)
      println("  About to sample for "+stepsPerBatch + " steps.")
      //for(mention <- evidenceBatch)println(EntityUtils.prettyPrintAuthor(mention))
      checkEntities
      mentionCount += evidenceBatch.filter(_.isObserved).size
      gtEntityCount = entities.filter((e:AuthorEntity) => {e.isObserved && e.groundTruth != None}).map(_.groundTruth.get).toSet.size
      process(stepsPerBatch)
      println("\n---DEBUG---")
      println("Evidence ended up:")
      evidenceBatch.foreach((e:AuthorEntity)=>EntityUtils.prettyPrintAuthor(e.entityRoot.asInstanceOf[AuthorEntity]))
      //model.familiesOfClass[HumanEditTemplate].head.debugFlag=true
      val debugged = new HashSet[AuthorEntity]
      for(e<-evidenceBatch){
        for(gf <- e.generatedFrom)
          println("Purity of edit source: "+EntityUtils.purity(gf))
        /*
        println("Checking affinity to generated-from entity")
        debugEditAffinityToGenerator(e)
        println("Checking should-link constraint")
        debugEditAffinityToLinked(e)
        println("Checking if edit worked as intended")
        debugEdit(e)
        println("Checking SNL constraint")
        debugEdit(e)
        */
        if(!debugged.contains(e)){
          debugged += e
          for(l<-e.linkedMention)debugged += l.asInstanceOf[AuthorEntity]
          debugSNLConstraint(e)
        }
      }
      //model.familiesOfClass[HumanEditTemplate].head.debugFlag=false
      //snapshot(totalTime,proposalCount,numAccepted,Evaluator.pairF1LabeledOnly(labeledData))
    }
    println("Streamed SNL statistics")
    printSNLStatistics
    reset
    println("Recomputing statistics")
    val debugged = new HashSet[AuthorEntity]
    for(evidenceBatch <- evidenceBatches){
      for(e<-evidenceBatch){
        if(!debugged.contains(e)){
          debugged += e
          for(l<-e.linkedMention)debugged += l.asInstanceOf[AuthorEntity]
          debugSNLConstraint(e)
        }
      }
    }
    println("Final SNL statistics")
    printSNLStatistics
  }

  /*
  def debugEditAffinityToLinked(e:AuthorEntity):Unit ={
    if(!(e.entityRoot eq e.linkedMention.get.entityRoot)){
      val d = new DebugDiffList
      this.mergeUp(e.entityRoot.asInstanceOf[AuthorEntity],e.linkedMention.get.entityRoot.asInstanceOf[AuthorEntity])(d)
      val score = d.scoreAndUndo(model)
      println("  Failed test 2. Affinity diff-score to link: "+score)
      if(score<=0.0)println("    test2 failure: model") else println("    test2 failure: inference")
    } else println("  Passed test 2: should link edits already in same entity.")
  }
  def debugEditAffinityToGenerator(e:AuthorEntity):Unit ={
    if(!(e.entityRoot eq e.generatedFrom.get.entityRoot)){
      val d = new DebugDiffList
      if(!e.generatedFrom.get.entityRoot.isLeaf)this.mergeLeft(e.generatedFrom.get.entityRoot.asInstanceOf[AuthorEntity],e)(d)
      else this.mergeUp(e.generatedFrom.get.asInstanceOf[AuthorEntity],e.entityRoot.asInstanceOf[AuthorEntity])(d)
      val score = d.scoreAndUndo(model)
      println("  Failed test 1. Affinity diff-score to generator: "+score)
      if(score<=0.0)println("    test1 failure: model") else println("    test1 failure: inference")
    } else println("  Passed test 1: edit in generated entity.")
  }
  def debugEdit(e:AuthorEntity):Unit ={
    if(!(e.entityRoot eq e.linkedMention.get.entityRoot) && !(e.entityRoot eq e.generatedFrom.get.entityRoot)){
      val d = new DebugDiffList
      if(!e.generatedFrom.get.entityRoot.isLeaf)this.mergeLeft(e.generatedFrom.get.entityRoot.asInstanceOf[AuthorEntity],e)(d)
      else this.mergeUp(e.generatedFrom.get.asInstanceOf[AuthorEntity],e.entityRoot.asInstanceOf[AuthorEntity])(d)
      this.mergeUp(e.entityRoot.asInstanceOf[AuthorEntity],e.linkedMention.get.entityRoot.asInstanceOf[AuthorEntity])(d)
      val score = d.scoreAndUndo(model)
      println("  Failed test 3. Edit did not accomplish merge: "+score)
      if(score<=0.0)println("    test3 failure: model") else println("    test3 failure: inference")
    } else println("  Passed test 3.")
  }
  */
  override def proposalHook(proposal:Proposal) ={
    super.proposalHook(proposal)
    curScore += proposal.modelScore
    if(curScore>maxScore)maxScore=curScore
    if(proposalCount % snapshotInterval == 0){
      val scores = Evaluator.pairF1LabeledOnly(labeledData)
      snapshot(totalTime,proposalCount,numAccepted,scores)
    }
  }
  def snapshot(time:Long,numSamples:Int,numAccepted:Int,scores:Iterable[Double]):Unit ={
    for(pw <- pwOption){
      val line = ((System.currentTimeMillis - time)/1000L + " "+numSamples+" "+numAccepted+" "+scores.mkString(" ")+" "+batchCount+" "+mentionCount+" "+gtEntityCount+" "+currentBatchName+" "+curScore+" "+maxScore)
      pw.println(line)
      pw.flush()
    }
  }
  def checkEntities:Unit ={
    val eids = new HashSet[String]
    eids ++= entities.map(_.id.toString)
    if(eids.size!=entities.size){
      throw new Exception("Error, duplicate entity detected: hashset:"+eids.size+" set: "+entities.size)
    }
  }
}

trait HumanEditOptions extends ExperimentOptions{
  val heExperimentType = new CmdOption("he-experiment-type","merge-correct","FILE","Experiment to run for human edits. Options are merge-correct, merge-incorrect, merge-all, split-correct, split-incorrect")
  val heShouldLinkReward = new CmdOption("he-should-link-reward",8.0,"DOUBLE","Should link reward for human edit template.")
  val heShouldNotLinkPenalty = new CmdOption("he-should-not-link-penalty",8.0,"DOUBLE","Should not link penalty for human edit template.")
  val heNumSynthesisSamples = new CmdOption("he-num-synthesis-samples",1000000,"DOUBLE","Should link reward for human edit template.")
  val heNumBatches = new CmdOption("he-num-batches",-1,"INT","Number of batches to stream. Default of -1 means n batches.")
  val heMergeExpEMinSize = new CmdOption("he-merge-exp-entity-min-size",3,"INT","Number of batches to stream. Default of -1 means n batches.")
  val heAdvanceSeed = new CmdOption("he-advance-seed",0,"INT","Number of times to call random.nextInt to advance the seed.")
  val heUseParallel = new CmdOption("he-use-parallel",false,"BOOL","If true, will use a parallel sampler to perform the initialization.")
  val heSaveInitialDB = new CmdOption("he-save-initial-db",false,"BOOL","If true, save the initial database.")
  val heInitializeEdits = new CmdOption("he-initialize-edits",false,"BOOL","If true, initialize edits to be in entities they were generated from.")
}

object EpiDBExperimentOptions extends MongoOptions with DataOptions with InferenceOptions with AuthorModelOptions with HumanEditOptions{
  val advanceSeed = new CmdOption("advance-seed",0,"INT","Number of times to call random.nextInt to advance the seed.")
  val outputFile = new CmdOption("outputFile","experiment.log","FILE","Output file for experimental results.")
  val outputDir = new CmdOption("outputDir","/Users/mwick/data/rexa2/experiments/","FILE","Root output directory containing intermediate results and final results")
  val scratchDir = new CmdOption("scratchDir","scratch/","FILE","Directory for intermediate results of experiment")
  val resultsDir = new CmdOption("resultsDir","results/","FILE","Directory for final results of experiment")
  val metaDir = new CmdOption("metaDir","meta/","FILE","Directory for meta information about results (parameters, settings, configurations used etc.)")
  val experimentName = new CmdOption("name","NONE","FILE","Name of experiment to run")
  val initialDBPercent = new CmdOption("initial-db-pct",0.5,"","Percentage of labeled data to include in the initial database")
  val numFolds = new CmdOption("num-folds",3,"","Number of folds for CV, this indirectly determines the size of the initial DB: initialDB.size=labeledData.size/numFolds")
  val fold = new CmdOption("fold",0,"","Specifies which fold to use as the inital DB.")
  val evidenceBatchSize = new CmdOption("evidence-batch-size",10,"","Size of each streaming batch of evidence")
  val inferenceStepsPerBatch = new CmdOption("inference-steps-per-batch",100000,"","Number of inference steps per batch of incoming evidence")
  val inferenceInitialSteps = new CmdOption("inference-steps-initial",0,"","Nubmer of steps of inference to run on the initial DB")
  val evidenceStreamType = new CmdOption("evidence-stream-type","random","","Types of evidence streams, current options are: random, byyear, human edits.")

  def main(argsIn:Array[String]):Unit ={
    var args:Array[String]=new Array[String](0)
    if(argsIn.length>0 && argsIn.head.startsWith("--config")){
      val contents = scala.io.Source.fromFile(new File(argsIn.head.split("=")(1))).mkString
      args = contents.split("\\s+") ++ argsIn
    } else args=argsIn
    println("Args: "+args.length)
    for(arg <- args)
      println("  "+arg)
    parse(args)
    //writeOptions(new File(this.metaDir.value+experimentName.value))
    if(ldaModel.wasInvoked)Coref.ldaFileOpt = Some(ldaModel.value)
    for(i<-0 until advanceSeed.value)random.nextInt
    if(dropDB.value){
      println("Dropping database.")
      val mongoConn = new Mongo(server.value,port.value.toInt)
      val mongoDB = mongoConn.getDB(database.value)
      mongoDB.getCollection("authors").drop
      mongoDB.getCollection("papers").drop
      mongoDB.getCollection("venues").drop
      mongoConn.close
    }
    println("server: "+server.value+" port: "+port.value.toInt+" database: "+database.value)
    val authorCorefModel = new AuthorCorefModel(false)
    def opts = this
    if(opts.entitySizeWeight.value != 0.0)authorCorefModel += new EntitySizePrior(opts.entitySizeWeight.value,opts.entitySizeExponent.value)
    if(opts.bagTopicsWeight.value != 0.0)authorCorefModel += new ChildParentCosineDistance[BagOfTopics](opts.bagTopicsWeight.value,opts.bagTopicsShift.value)
    if(opts.bagTopicsEntropy.value != 0.0)authorCorefModel += new EntropyBagOfWordsPriorWithStatistics[BagOfTopics](opts.bagTopicsEntropy.value)
    if(opts.bagTopicsPrior.value != 0.0)authorCorefModel += new BagOfWordsPriorWithStatistics[BagOfTopics](opts.bagTopicsPrior.value)
    if(opts.bagCoAuthorWeight.value != 0.0)authorCorefModel += new ChildParentCosineDistance[BagOfCoAuthors](opts.bagCoAuthorWeight.value,opts.bagCoAuthorShift.value)
    if(opts.bagCoAuthorEntropy.value != 0.0)authorCorefModel += new EntropyBagOfWordsPriorWithStatistics[BagOfCoAuthors](opts.bagCoAuthorEntropy.value)
    if(opts.bagCoAuthorPrior.value != 0.0)authorCorefModel += new BagOfWordsPriorWithStatistics[BagOfCoAuthors](opts.bagCoAuthorPrior.value)
    if(opts.bagVenuesWeight.value != 0.0)authorCorefModel += new ChildParentCosineDistance[BagOfVenues](opts.bagVenuesWeight.value,opts.bagVenuesShift.value)
    if(opts.bagVenuesEntropy.value != 0.0)authorCorefModel += new EntropyBagOfWordsPriorWithStatistics[BagOfVenues](opts.bagVenuesEntropy.value)
    if(opts.bagVenuesPrior.value != 0.0)authorCorefModel += new BagOfWordsPriorWithStatistics[BagOfVenues](opts.bagVenuesPrior.value)
    if(opts.bagKeywordsWeight != 0.0)authorCorefModel += new ChildParentCosineDistance[BagOfKeywords](opts.bagKeywordsWeight.value,opts.bagKeywordsShift.value)
    if(opts.bagKeywordsEntropy.value != 0.0)authorCorefModel += new EntropyBagOfWordsPriorWithStatistics[BagOfKeywords](opts.bagKeywordsEntropy.value)
    if(opts.bagKeywordsPrior.value != 0.0)authorCorefModel += new BagOfWordsPriorWithStatistics[BagOfKeywords](opts.bagKeywordsPrior.value)
    if(opts.entityExistencePenalty.value!=0.0 && opts.subEntityExistencePenalty.value!=0.0)authorCorefModel += new StructuralPriorsTemplate(opts.entityExistencePenalty.value, opts.subEntityExistencePenalty.value)
    //
    if(opts.bagFirstWeight.value != 0.0)authorCorefModel += new EntityNameTemplate[BagOfFirstNames](opts.bagFirstInitialWeight.value,opts.bagFirstNameWeight.value,opts.bagFirstWeight.value,opts.bagFirstSaturation.value)
    if(opts.bagMiddleWeight.value != 0.0)authorCorefModel += new EntityNameTemplate[BagOfMiddleNames](opts.bagMiddleInitialWeight.value,opts.bagMiddleNameWeight.value,opts.bagMiddleWeight.value,opts.bagMiddleSaturation.value)

    val epiDB = new EpistemologicalDB(authorCorefModel,server.value,port.value.toInt,database.value)
    println("About to add data.")
    var papers = new ArrayBuffer[PaperEntity]
    if(bibDirectory.value.toLowerCase != "none"){
      println("Adding mentions from BibTeX directory: "+bibDirectory.value.toLowerCase)
      papers ++= BibReader.loadBibTexDirMultiThreaded(new File(bibDirectory.value),true,false)
      println("  total papers: "+papers.size)
    }
    if(rexaData.value.toLowerCase != "none"){
      println("Loading labeled data from: " + rexaData.value)
      papers ++= RexaLabeledLoader.load(new File(rexaData.value))
      println("  total papers: "+papers.size)
    }
    if(dblpLocation.value.toLowerCase != "none"){
      println("Loading dblp data from: "+dblpLocation.value)
      papers ++= DBLPLoader.loadDBLPData(dblpLocation.value)
      println("  total papers: "+papers.size)
    }
    println("About to add "+papers.size+" papers.")
    epiDB.add(papers)
    println("Finished adding papers.")
    var authors:Seq[AuthorEntity] = random.shuffle(epiDB.authorColl.loadLabeledAndCanopies)
    var initialDB:Seq[AuthorEntity] = null
    var evidenceBatches:Seq[Seq[AuthorEntity]] = null
    var evidenceBatchNamesOpt:Option[Seq[String]] = None
    var initialDBNameOpt:Option[String] = None
    var initInstructionsOpt:Option[Seq[Seq[()=>Unit]]] = None
    println("Authors.size" +authors.size)
    EntityUtils.checkIntegrity(authors)
    Evaluator.eval(authors)
    println("Evidence stream: "+evidenceStreamType.value)
    if(!evidenceStreamType.wasInvoked)throw new Exception("Remember to specify the type of evidence you want to stream.")
    if(evidenceStreamType.value=="human-edits"){
      for(i<-0 until heAdvanceSeed.value)random.nextInt
      //do human edit experiment
      val humanEditTemplate = new HumanEditTemplate(opts.heShouldLinkReward.value,opts.heShouldNotLinkPenalty.value)
      authorCorefModel += humanEditTemplate
      opts.heExperimentType.value match{
        case "merge-correct" =>{
          authors = authors.filter((a:AuthorEntity) => {a.groundTruth != None || a.bagOfTruths.size>0})
          //create evidence stream by running inference, then considering all possible merges that would increase F1 score
          val samplerForCreatingInitialDB = if(heUseParallel.value) new ParallelAuthorSampler(authorCorefModel, 5){temperature=0.001} else new AuthorSampler(authorCorefModel){temperature = 0.001}
          samplerForCreatingInitialDB.setEntities(authors)
          samplerForCreatingInitialDB.timeAndProcess(opts.heNumSynthesisSamples.value)
          initialDB = samplerForCreatingInitialDB.getEntities
          if(heSaveInitialDB.value){
            epiDB.authorColl.store(samplerForCreatingInitialDB.getEntities ++ samplerForCreatingInitialDB.getDeletedEntities)
            println("Human edits merge-correct: saved initial db and exiting.")
            System.exit(0)
          }
          val edits = HumanEditExperiments.getAuthorEdits(initialDB,opts.heMergeExpEMinSize.value).filter(_.isCorrect)
          evidenceBatches = HumanEditExperiments.edits2evidenceBatches(edits,opts.heNumBatches.value)
          evidenceBatches = random.shuffle(evidenceBatches)
          HumanEditExperiments.mergeBaseline1(initialDB,evidenceBatches,new File(outputFile.value+".baseline1"))
          HumanEditExperiments.mergeBaseline2(initialDB,evidenceBatches,new File(outputFile.value+".baseline2"))
        }
        case "merge-incorrect" => {
          authors = authors.filter((a:AuthorEntity) => {a.groundTruth != None || a.bagOfTruths.size>0})
          //create evidence stream by running inference, then considering all possible merges that would increase F1 score
          val samplerForCreatingInitialDB = if(heUseParallel.value) new ParallelAuthorSampler(authorCorefModel, 5){temperature=0.001} else new AuthorSampler(authorCorefModel){temperature = 0.001}
          samplerForCreatingInitialDB.setEntities(authors)
          samplerForCreatingInitialDB.timeAndProcess(opts.heNumSynthesisSamples.value)
          initialDB = samplerForCreatingInitialDB.getEntities
          if(heSaveInitialDB.value){
            epiDB.authorColl.store(samplerForCreatingInitialDB.getEntities ++ samplerForCreatingInitialDB.getDeletedEntities)
            println("Human edits merge-incorrect: saved initial db and exiting.")
            System.exit(0)
          }
          val edits = HumanEditExperiments.getAuthorEdits(initialDB,opts.heMergeExpEMinSize.value).filter(! _.isCorrect)
          evidenceBatches = HumanEditExperiments.edits2evidenceBatches(edits,opts.heNumBatches.value)
          evidenceBatches = random.shuffle(evidenceBatches)
          HumanEditExperiments.mergeBaseline1(initialDB,evidenceBatches,new File(outputFile.value+".baseline1"))
          HumanEditExperiments.mergeBaseline2(initialDB,evidenceBatches,new File(outputFile.value+".baseline2"))
        }
        case "merge-mixed" => {
          authors = authors.filter((a:AuthorEntity) => {a.groundTruth != None || a.bagOfTruths.size>0})
          //create evidence stream by running inference, then considering all possible merges that would increase F1 score
          val samplerForCreatingInitialDB = if(heUseParallel.value) new ParallelAuthorSampler(authorCorefModel, 5){temperature=0.001} else new AuthorSampler(authorCorefModel){temperature = 0.001}
          samplerForCreatingInitialDB.setEntities(authors)
          samplerForCreatingInitialDB.timeAndProcess(opts.heNumSynthesisSamples.value)
          initialDB = samplerForCreatingInitialDB.getEntities
          if(heSaveInitialDB.value){
            epiDB.authorColl.store(samplerForCreatingInitialDB.getEntities ++ samplerForCreatingInitialDB.getDeletedEntities)
            println("Human edits merge-mixed: saved initial db and exiting.")
            System.exit(0)
          }
          val edits = HumanEditExperiments.getAuthorEdits(initialDB,opts.heMergeExpEMinSize.value).filter(! _.isCorrect)
          val correctEdits = edits.filter(_.isCorrect)
          val incorrectEdits = random.shuffle(edits.filter(! _.isCorrect)).take(correctEdits.size)
          evidenceBatches = HumanEditExperiments.edits2evidenceBatches(correctEdits++incorrectEdits,opts.heNumBatches.value)
          evidenceBatches = random.shuffle(evidenceBatches)
          HumanEditExperiments.mergeBaseline1(initialDB,evidenceBatches,new File(outputFile.value+".baseline1"))
          HumanEditExperiments.mergeBaseline2(initialDB,evidenceBatches,new File(outputFile.value+".baseline2"))
        }
        case "split-correct" => {
          val initDiffList = new DiffList
          authors = authors.filter((a:AuthorEntity) => {a.groundTruth != None || a.bagOfTruths.size>0})
          //create evidence stream by running inference, then considering all possible merges that would increase F1 score
          val samplerForCreatingInitialDB = if(heUseParallel.value) new ParallelAuthorSampler(authorCorefModel, 5){temperature=0.001} else new AuthorSampler(authorCorefModel){temperature = 0.001}
          samplerForCreatingInitialDB.setEntities(authors)
          samplerForCreatingInitialDB.timeAndProcess(opts.heNumSynthesisSamples.value)
          initialDB = samplerForCreatingInitialDB.getEntities
          if(heSaveInitialDB.value){
            epiDB.authorColl.store(samplerForCreatingInitialDB.getEntities ++ samplerForCreatingInitialDB.getDeletedEntities)
            println("Human edits split-correct: saved initial db and exiting.")
            System.exit(0)
          }
          val edits = HumanEditExperiments.correctAuthorSplitEdits(initialDB)
          evidenceBatches = HumanEditExperiments.edits2evidenceBatches(edits,opts.heNumBatches.value)
          evidenceBatches = random.shuffle(evidenceBatches)
          if(opts.heInitializeEdits.value){
            val initInstructions = new ArrayBuffer[ArrayBuffer[()=>Unit]]
            for(batch <- evidenceBatches){
              val ibatch = new ArrayBuffer[()=>Unit]
              initInstructions + ibatch
              for(edit <- batch){
                ibatch += {() => {
                  println("linking edit to generator: "+edit.id)
                  println("   is parent null? "+(edit.parentEntity==null))
                  if(!edit.generatedFrom.get.isObserved)EntityUtils.linkChildToParent(edit,edit.generatedFrom.get)(null)
                  //EntityUtils.linkChildToParent(edit.linkedMention.get,edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get)(null)
                }}
              }
            }
            initInstructionsOpt = Some(initInstructions)
            /*
            for(edit <- edits.flatMap(_.mentions)){
              if(edit.parentEntity == null){
                EntityUtils.linkChildToParent(edit,edit.generatedFrom.get)(initDiffList)
                EntityUtils.linkChildToParent(edit.linkedMention.get,edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get)(initDiffList)
              }
            }
            */
          }
          HumanEditExperiments.splitBaseline1(initialDB,evidenceBatches,new File(outputFile.value+".baseline1"))
          //HumanEditExperiments.applySplitEdits(evidenceBatches)(initDiffList)
          //println("Diff score after applying split edits: "+initDiffList.score(authorCorefModel))
        }
        case "split-incorrect" => {
          val initDiffList = new DiffList
          authors = authors.filter((a:AuthorEntity) => {a.groundTruth != None || a.bagOfTruths.size>0})
          val samplerForCreatingInitialDB = if(heUseParallel.value) new ParallelAuthorSampler(authorCorefModel, 5){temperature=0.001} else new AuthorSampler(authorCorefModel){temperature = 0.001}
          samplerForCreatingInitialDB.setEntities(authors)
          samplerForCreatingInitialDB.timeAndProcess(opts.heNumSynthesisSamples.value)
          initialDB = samplerForCreatingInitialDB.getEntities
          val edits = HumanEditExperiments.incorrectAuthorSplitEdits(initialDB)
          evidenceBatches = HumanEditExperiments.edits2evidenceBatches(edits,opts.heNumBatches.value)
          evidenceBatches = random.shuffle(evidenceBatches)
          if(opts.heInitializeEdits.value){
            val initInstructions = new ArrayBuffer[ArrayBuffer[()=>Unit]]
            for(batch <- evidenceBatches){
              val ibatch = new ArrayBuffer[()=>Unit]
              initInstructions + ibatch
              for(edit <- batch){
                ibatch += {() => {
                  println("linking edit to generator: "+edit.id)
                  println("   is parent null? "+(edit.parentEntity==null))
                  if(!edit.generatedFrom.get.isObserved)EntityUtils.linkChildToParent(edit,edit.generatedFrom.get)(null)
                  //EntityUtils.linkChildToParent(edit.linkedMention.get,edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get)(null)
                }}
              }
            }
            initInstructionsOpt = Some(initInstructions)
            /*
            for(edit <- edits.flatMap(_.mentions)){
              if(edit.parentEntity == null){
                EntityUtils.linkChildToParent(edit,edit.generatedFrom.get)(initDiffList)
                EntityUtils.linkChildToParent(edit.linkedMention.get,edit.linkedMention.get.asInstanceOf[AuthorEntity].generatedFrom.get)(initDiffList)
              }
            }
            */
          }
          HumanEditExperiments.splitBaseline1(initialDB,evidenceBatches,new File(outputFile.value+".baseline1"))
          //HumanEditExperiments.applySplitEdits(evidenceBatches)(initDiffList)
          //println("Diff score after applying split edits: "+initDiffList.score(authorCorefModel))
        }
        case "mixed" => {
          authors = authors.filter((a:AuthorEntity) => {a.groundTruth != None || a.bagOfTruths.size>0})
          //create evidence stream by running inference, then considering all possible merges that would increase F1 score
          val samplerForCreatingInitialDB = if(heUseParallel.value) new ParallelAuthorSampler(authorCorefModel, 5){temperature=0.001} else new AuthorSampler(authorCorefModel){temperature = 0.001}
          samplerForCreatingInitialDB.setEntities(authors)
          samplerForCreatingInitialDB.timeAndProcess(opts.heNumSynthesisSamples.value)
          initialDB = samplerForCreatingInitialDB.getEntities
          if(heSaveInitialDB.value){
            epiDB.authorColl.store(samplerForCreatingInitialDB.getEntities ++ samplerForCreatingInitialDB.getDeletedEntities)
            println("Human edits merge-correct: saved initial db and exiting.")
            System.exit(0)
          }
          val correctMergeEdits = HumanEditExperiments.getAuthorEdits(initialDB,opts.heMergeExpEMinSize.value).filter(_.isCorrect)
          val correctSplitEdits = HumanEditExperiments.correctAuthorSplitEdits(initialDB)
          val edits = correctMergeEdits ++ correctSplitEdits
          evidenceBatches = HumanEditExperiments.edits2evidenceBatches(edits,opts.heNumBatches.value)
          evidenceBatches = random.shuffle(evidenceBatches)
          //HumanEditExperiments.mergeBaseline1(initialDB,evidenceBatches,new File(outputFile.value+".baseline1"))
          //HumanEditExperiments.mergeBaseline2(initialDB,evidenceBatches,new File(outputFile.value+".baseline2"))
        }
        case _ => throw new Exception("Human edit experiment type "+opts.heExperimentType.value + " not implemented.")
      }
      println("Finished generating  human edit experiment")
    }
    else if(this.evidenceStreamType.value=="random"){
      val (initialDB2,evidence) = split(authors,numFolds.value,fold.value)
      initialDB=initialDB2
      println("initialDB.size "+initialDB.size+" evidence.size: "+evidence.size)
      evidence.foreach(_.groundTruth=None) //so it `won't get evaluated during the experiment
      evidenceBatches = randomEqualPartitioning(evidence,evidenceBatchSize.value)
    }
    else if(this.evidenceStreamType.value=="byyear"){
      val years = this.partitionByYear(authors)
      println("Paper counts by year.")
      println("   year: count")
      var yearCount = 0
      for((k,v) <- years.toList.sortBy(_._1)){
        println("   "+k+": "+v.size)
        if(k != -1)yearCount += v.size
      }
      println("Number of papers with year: "+yearCount)
      val targetSize = yearCount/numFolds.value
      val initialDBBuffer = new ArrayBuffer[AuthorEntity]
      val evidenceBatchesBuffer = new ArrayBuffer[Seq[AuthorEntity]]
      val evidenceBatchNames = new ArrayBuffer[String]
      evidenceBatchNamesOpt = Some(evidenceBatchNames)
      var initialDBName:String = ""
      for((k,v) <- years.toList.sortBy(_._1)){
        if(k != -1){
          if(initialDBBuffer.size<targetSize){
            if(initialDBName.length==0)initialDBName = k.toString
            println("Adding year "+ k + " to initial DB.")
            initialDBBuffer ++= v
          } else {
            if(initialDBName.length<=4)initialDBName += "-"+k.toString
            evidenceBatchesBuffer += v
            evidenceBatchNames += k.toString
          }
        }
      }
      initialDBNameOpt=Some(initialDBName)
      println("Initial DB name: "+initialDBName)
      if(years.contains(-1)){
        evidenceBatchesBuffer += years(-1)
        evidenceBatchNames += "????"
      }
      initialDB = initialDBBuffer
      evidenceBatches = evidenceBatchesBuffer
      println("  initialDB.size: " + initialDB.size)
      println("  num evidence batches: "+evidenceBatches.size)
    }
    else println("unrecognized evidence stream: "+evidenceStreamType.value)
    val sampler = new AuthorSamplerWriter(authorCorefModel,initialDB,evidenceBatches,initialDBNameOpt,evidenceBatchNamesOpt,inferenceInitialSteps.value,inferenceStepsPerBatch.value,initInstructionsOpt){temperature = 0.001}
    sampler.processExperiment(new PrintWriter(new File(outputFile.value)))
  }
  def split[T](seq:Seq[T],numFolds:Int,fold:Int):(Seq[T],Seq[T]) = {
    val folds = randomEqualPartitioning(seq,(scala.math.ceil(seq.size.toDouble/numFolds.toDouble)).toInt)
    val other = new ArrayBuffer[T]
    for(i<-0 until folds.size)if(i!=fold)other ++= folds(i)
    (folds(fold),other)
  }
  def randomSubset[T](seq:Seq[T],pct:Double):Seq[T] = randomSubset[T](seq,(seq.size.toDouble*pct).toInt)
  def randomSubset[T](seq:Seq[T],n:Int):Seq[T] = random.shuffle(seq).take(n)
  def randomEqualPartitioning[T](seq:Seq[T],partitionSize:Int):Seq[Seq[T]] = {
    val result = new ArrayBuffer[Seq[T]]
    var batch = new ArrayBuffer[T]
    random.shuffle(seq)
    var i =0
    while(i<seq.size){
      if(i % partitionSize==0){
        batch = new ArrayBuffer[T]
        result += batch
      }
      batch += seq(i)
      i+=1
    }
    println("SEQ SIZE: "+seq.size+" batches: " + result.size)
    result
  }
  def partitionByYear[T<:Attr](seq:Seq[T]):HashMap[Int,Seq[T]] ={
    val result = new HashMap[Int,ArrayBuffer[T]]
    for(s <- seq){
      val b = result.getOrElseUpdate(s.attr[Year].intValue,new ArrayBuffer[T])
      b += s
    }
    result.asInstanceOf[HashMap[Int,Seq[T]]]
  }
}
trait ExperimentOptions extends DefaultCmdOptions{
  def writeOptions(file:File):Unit = {
    //if(!file.exists)file.mkDirs
    val pw = new PrintWriter(file)
    writeOptions(pw)
    pw.close
  }
  def writeOptions(pw:PrintWriter):Unit ={
    //pw.println("Experiment Parameters: "+DateFormat.getDateInstance(DateFormat.SHORT).format(now))
    this.foreach(o => pw.println("--"+o.name+"="+o.value))
    pw.flush()
  }
  def readOptions(file:File):Unit ={
    import scala.io.Source
    val contents = Source.fromFile(file).mkString
    val args = contents.split("\\s+")
    parse(args)
  }
}
trait MongoOptions extends ExperimentOptions{
  val server = new CmdOption("server","localhost","FILE","Location of Mongo server.")
  val port = new CmdOption("port","27017","FILE","Port of Mongo server.")
  val database = new CmdOption("database","rexa2-cubbies","FILE","Name of mongo database.")
}
trait DataOptions extends ExperimentOptions{
  val bibDirectory = new CmdOption("bibDir","/Users/mwick/data/thesis/all3/","FILE","Pointer to a directory containing .bib files.")
  val rexaData = new CmdOption("rexaData","/Users/mwick/data/rexa/rexaAll/","FILE","Location of the labeled rexa2 directory.")
  val dblpLocation = new CmdOption("dblpFile","none","FILE","Location of DBLP xml file.")
  val aronData = new CmdOption("aronData","/data/thesis/rexa1/rexa_coref_datav0.5/","FILE","Location of Aron's labeled data")
  val ldaModel = new CmdOption("ldaModel","lda-model.txt","FILE","Location of lda model")
  val filterPapersOnLabeledCanopies = new CmdOption("filter-for-labeled",false,"FILE","True: only insert papers into the DB that contain an author in a canopy of another labeled mention")
  val saveDB = new CmdOption("saveDB",true,"BOOLEAN","true: saves inference results, false: discard inference results")
  val dropDB = new CmdOption("dropDB",true,"BOOLEAN","true: saves inference results, false: discard inference results")

}
trait InferenceOptions extends ExperimentOptions{
  //inference options
  val numEpochs = new CmdOption("epochs","1","FILE","Number of inference round-trips to DB.")
  val batchSize = new CmdOption("batchSize","10000","FILE","Number of entities used to retrieve canopies from.")
  val stepMultiplierA = new CmdOption("a","0.0","FILE","Runs for n^2 steps (n=number of mentions to do inference on.)")
  val stepMultiplierB = new CmdOption("b","0.0","FILE","Runs for n steps (n=number of mentions to do inference on.)")
  val stepMultiplierC = new CmdOption("c","1000000.0","FILE","Runs for c steps (c=constant)")
  val evaluateOnly = new CmdOption("evaluate","false","FILE","Loads labeled data, evaluates the accuracy of coreference, and exits.")
  //model
}
trait AuthorModelOptions extends ExperimentOptions{
  //co-authors
  val bagCoAuthorWeight = new CmdOption("model-author-bag-coauthors-weight", 4.0, "N", "Penalty for bag-of-co-authors cosine distance template (author coreference model).")
  val bagCoAuthorShift = new CmdOption("model-author-bag-coauthors-shift", -0.125, "N", "Shift for bag-of-co-authors cosine distance template  (author coreference model).")
  val bagCoAuthorEntropy = new CmdOption("model-author-bag-coauthors-entropy", 0.125, "N", "Penalty on bag-of-co-author entropy (author coreference model).")
  val bagCoAuthorPrior = new CmdOption("model-author-bag-coauthors-prior", 0.25, "N", "Bag of co-author prior penalty, formula is bag.size/bag.oneNorm*weight.")
  //venues
  val bagVenuesWeight = new CmdOption("model-author-bag-venues-weight", 4.0, "N", "Penalty for bag-of-venues cosine distance template (the author coreference model).")
  val bagVenuesShift = new CmdOption("model-author-bag-venues-shift", -0.125, "N", "Shift for bag-of-venues cosine distance template (author coreference model).")
  val bagVenuesEntropy = new CmdOption("model-author-bag-venues-entropy", 0.125, "N", "Penalty on bag-of-venue entropy (author coreference model).")
  val bagVenuesPrior = new CmdOption("model-author-bag-venues-prior", 0.25, "N", "Bag of co-author prior penalty, formula is bag.size/bag.oneNorm*weight.")
  //keywords
  val bagKeywordsWeight = new CmdOption("model-author-bag-keywords-weight", 2.0, "N", "Penalty for bag-of-keywords template  (the author coreference model).")
  val bagKeywordsShift = new CmdOption("model-author-bag-keywords-shift", -0.125, "N", "Bag-of-keywords shift for  (author coreference model).")
  val bagKeywordsEntropy = new CmdOption("model-author-bag-keywords-entropy", 0.25, "N", "Penalty on bag of keywrods entropy(author coreference model).")
  val bagKeywordsPrior = new CmdOption("model-author-bag-keywords-prior", 0.25, "N", "Bag of co-author prior penalty, formula is bag.size/bag.oneNorm*weight.")
  //topics
  val bagTopicsWeight = new CmdOption("model-author-bag-topics-weight", 4.0, "N", "Penalty for bag-of-topics cosine distance template (author coreference model).")
  val bagTopicsShift = new CmdOption("model-author-bag-topics-shift", -0.25, "N", "Shift for bag-of-topics cosine distance template (author coreference model).")
  val bagTopicsEntropy = new CmdOption("model-author-bag-topics-entropy", 0.75, "N", "Penalty on bag of topics entropy  (author coreference model).")
  val bagTopicsPrior = new CmdOption("model-author-bag-topics-prior", 0.25, "N", "Bag of co-author prior penalty, formula is bag.size/bag.oneNorm*weight.")
  val entitySizeExponent = new CmdOption("model-author-size-prior-exponent", 1.2, "N", "Exponent k for rewarding entity size: w*|e|^k")
  val entitySizeWeight = new CmdOption("model-author-size-prior-weight", 0.05, "N", "Weight w for rewarding entity size: w*|e|^k.")
  //author names
  val bagFirstInitialWeight = new CmdOption("model-author-bag-first-initial-weight", 3.0, "N", "Penalty for first initial mismatches.")
  val bagFirstNameWeight = new CmdOption("model-author-bag-first-name-weight", 3.0, "N", "Penalty for first name mismatches")
  val bagFirstSaturation = new CmdOption("model-author-bag-first-saturation", 16.0, "N", "Penalty for first initial mismatches.")
  val bagFirstWeight = new CmdOption("model-author-bag-first-weight", 1.0, "N", "Penalty for first initial mismatches.")
  val bagMiddleInitialWeight = new CmdOption("model-author-bag-middle-initial-weight", 3.0, "N", "Penalty for first initial mismatches.")
  val bagMiddleNameWeight = new CmdOption("model-author-bag-middle-name-weight", 3.0, "N", "Penalty for first name mismatches")
  val bagMiddleSaturation = new CmdOption("model-author-bag-middle-saturation", 26.0, "N", "Penalty for first initial mismatches.")
  val bagMiddleWeight = new CmdOption("model-author-bag-middle-weight", 1.0, "N", "Penalty for first initial mismatches.")

  //structural priors
  val entityExistencePenalty = new CmdOption("model-author-entity-penalty", 2.0, "N", "Penalty for a top-level author entity existing")
  val subEntityExistencePenalty = new CmdOption("model-author-subentity-penalty", 0.25, "N", "Penalty for an author subentity existing")
  val bagFirstNamePenalty = new CmdOption("model-author-firstname-penalty", 16.0, "N", "Penalty for having multiple first names")
  val bagMiddleNamePenalty = new CmdOption("model-author-middlename-penalty", 16.0, "N", "Penalty for having multiple middle names")
}

trait PaperModelOptions extends ExperimentOptions{
  val paperBagOfTitlesWeight = new CmdOption("model-paper-bag-titles-weight", 4.0, "N", "Penalty for bag-of-papers cosine distance template (paper coreference model).")
  val paperBagOfTitlesShift = new CmdOption("model-paper-bag-titles-shift", -0.125, "N", "Shift for bag-of-papers cosine distance template (paper coreference model).")
  val paperBagTitlesEntropy = new CmdOption("model-paper-bag-titles-entropy", 0.0, "N", "Entropy penalty for bag-of-titles cosine distance template (paper coreference model).")
  val paperBagTitlesPrior = new CmdOption("model-paper-bag-titles-prior", 0.0, "N", "Prior for bag-of-titles cosine distance template (paper coreference model).")
  val paperBagOfAuthorsWeight = new CmdOption("model-paper-bag-authors-weight", 4.0, "N", "Penalty for bag-of-authors cosine distance template (paper coreference model).")
  val paperBagOfAuthorsShift = new CmdOption("model-paper-bag-authors-shift", -0.125, "N", "Shift for bag-of-authors cosine distance template (paper coreference model).")
  val paperBagAuthorsEntropy = new CmdOption("model-paper-bag-authors-entropy", 0.0, "N", "Entropy penalty for bag-of-authors cosine distance template (paper coreference model).")
  val paperBagAuthorsPrior = new CmdOption("model-paper-bag-authors-prior", 0.0, "N", "Prior for bag-of-authors cosine distance template (paper coreference model).")
  val paperYearPenalty = new CmdOption("model-paper-year-penalty-title", 4, "N", "Penalizes mismatching years (paper coreference model).")
  val paperEntityExistencePenalty = new CmdOption("model-paper-entity-penalty", 2.0, "N", "Penalty for a top-level paper entity existing (paper coreference model).")
  val paperSubEntityExistencePenalty = new CmdOption("model-paper-subentity-penalty", 0.25, "N", "Penalty for a paper subentity existing (paper coreference model).")
}