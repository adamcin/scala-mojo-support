package org.scala_tools.maven.mojo.extractor

import org.apache.maven.plugin.descriptor.MojoDescriptor
import org.apache.maven.plugin.descriptor.Parameter
import org.scala_tools.maven.mojo.annotations._
import org.apache.maven.tools.plugin.ExtendedMojoDescriptor
import org.apache.maven.project.MavenProject
import org.apache.maven.execution.MavenSession
import org.codehaus.plexus.component.repository.ComponentRequirement
;


/**
 * Helper class that can extract a MojoDescriptor from a MojoClassInfo object.
 */
trait MojoExtractionInfo {
  def extractMojoDescriptor(mojoInfo : MojoClassInfo) : MojoDescriptor = {
    val desc = new ExtendedMojoDescriptor()
    desc.setLanguage("scala");
    desc.setComponentConfigurator("scala");
    desc.setImplementation(mojoInfo.name)
    // Add mojo annotations
    for(annotation <- mojoInfo.annotations) {
      annotation match {
        case goal(name) =>
          desc.setGoal(name)
          desc.setExecuteGoal(name)
        case phase(name) =>
          desc.setPhase(name)
          desc.setExecutePhase(name)
        case configurator(roleHint) =>
          desc.setComponentConfigurator(roleHint)
        case executeGoal(value) =>
          desc.setExecuteGoal(value)
        case executePhase(value) =>
          desc.setExecutePhase(value)
        case executePhaseInLifecycle(phase, lifeCycle) =>
          desc.setExecutePhase(phase)
          desc.setExecuteLifecycle(lifeCycle)
        //TODO -= Figure this one out
//        case executionStrategy =>
        case inheritByDefault(value) =>
          desc.setInheritedByDefault(value)
        case instantiationStrategy(value) =>
          desc.setInstantiationStrategy(value)
        case requiresDependencyResolution(scope) =>
          desc.setDependencyResolutionRequired(scope)
        case requiresDirectInvocation(value) =>
          desc.setDirectInvocationOnly(value)
        case requiresOnline(value) =>
          desc.setOnlineRequired(value)
        case requiresProject(value) =>
          desc.setProjectRequired(value)
        case requiresReports(value) =>
          desc.setRequiresReports(value)
        case description(value) =>
          desc.setDescription(value)
        case since(value) =>
          desc.setSince(value)
        case requiresDependencyCollection(value) =>
          desc.setDependencyCollectionRequired(value)
        case threadSafe(value) =>
          desc.setThreadSafe(value)
        case _ => //ignore
      }
    }
    //Add all parameters
    for(param <- mojoInfo.parameters) {
      val paramInfo = new Parameter()
      paramInfo.setName(param.name)
      paramInfo.setType(param.typeClass)
      //TODO - Set implementation to type?
      for(annotation <- param.annotations) {
        annotation match {
          case required() => paramInfo.setRequired(true)
          case readOnly() => paramInfo.setEditable(false)
          case expression(value) => paramInfo.setExpression(value)
          case defaultValue(value) => paramInfo.setDefaultValue(value)
          case alias(value) => paramInfo.setAlias(value)
          //TODO Add requirement
          case component(role, roleHint) => {
            if (role == classOf[MavenProject].getName) {
              paramInfo.setImplementation(classOf[MavenProject].getName)
              paramInfo.setDefaultValue("${project}")
              paramInfo.setRequired(true)
              paramInfo.setEditable(false)
            } else if (role == classOf[MavenSession].getName) {
              paramInfo.setImplementation(classOf[MavenSession].getName)
              paramInfo.setDefaultValue("${session}")
              paramInfo.setRequired(true)
              paramInfo.setEditable(false)
            } else {
              val req = new ComponentRequirement
              req.setFieldName(param.name)
              Option(role) match {
                case Some("") => req.setRole(param.typeClass)
                case None => req.setRole(param.typeClass)
                case Some(r) => req.setRole(r)
              }
              Option(roleHint) match {
                case Some("") => ()
                case None => ()
                case Some(rh) => req.setRoleHint(roleHint)
              }
              desc.addRequirement(req)
            }
          }
          case description(value) => paramInfo.setDescription(value)
          //TODO Add deprecated support
//          case deprecated =>
          case since(value) => paramInfo.setSince(value)
          case _ => //Ignore
        }
      } 
      desc.addParameter(paramInfo)
    }    
    desc
  }
  
  
}



//Class information string
case class MojoClassInfo(name: String, annotations: List[MavenAnnotation], parameters: List[MojoInjectedVarInfo])

case class MojoInjectedVarInfo(name: String, typeClass: String, annotations : List[MavenAnnotation])
