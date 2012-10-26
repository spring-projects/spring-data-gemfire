/*
 * Copyright 2002-2011 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.function;

import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.util.ClassUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.ReflectionUtils;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.execute.Function;
import com.gemstone.gemfire.cache.execute.FunctionContext;
import com.gemstone.gemfire.cache.execute.FunctionException;
import com.gemstone.gemfire.cache.execute.RegionFunctionContext;
import com.gemstone.gemfire.cache.execute.ResultSender;
import com.gemstone.gemfire.cache.partition.PartitionRegionHelper;

/**
 * Invokes a POJO's given method as a Gemfire remote function. 
 * If the POJO has a constructor that takes a Map, and the function context is Region, the 
 * region will be injected. The delegate class name, the method name, and the method arguments
 * are part of a remote function invocation, therefore all arguments must be serializable. 
 * The delegate class must be the class path of the remote cache(s)
 * @author David Turanski
 *
 */
@SuppressWarnings("serial")
public class MethodInvokingFunction implements Function {
	public static final String FUNCTION_ID = MethodInvokingFunction.class.getName();
	private static transient Log logger = LogFactory.getLog(MethodInvokingFunction.class);
 
	private volatile boolean HA;
	private volatile boolean optimizeForWrite;
	private volatile boolean hasResult;
	private String id;

	 
	
	public MethodInvokingFunction( ) {
		this.HA = false;
		this.hasResult = true;
		this.optimizeForWrite = false;
	}
	
	//@Override	 
	public String getId() {
		return this.id;
	}
	
	//@Override	 
	public boolean hasResult() {
		return hasResult;
	}

	public void setHasResult(boolean hasResult) {
		this.hasResult = hasResult;
	}

	//@Override	
	public boolean isHA() {
		return HA;
	}

	public void setHA(boolean HA) {
		this.HA = HA;
	}
	
	public void setId(String id) {
		this.id = id;
	}

	//@Override	
	public boolean optimizeForWrite() {
		return optimizeForWrite;
	}

 
	public void setOptimizeForWrite(boolean optimizeForWrite) {
		this.optimizeForWrite = optimizeForWrite;
	}
    
	//@Override
	public void execute(FunctionContext functionContext) {
		Region<?,?> region = null;
		RegionFunctionContext regionFunctionContext = (RegionFunctionContext)functionContext;
		 
		region = getRegionForContext(regionFunctionContext);
	
		RemoteMethodInvocation invocation = null;
	
		if (regionFunctionContext.getArguments().getClass().isArray()) {
			invocation = (RemoteMethodInvocation)((Serializable[])regionFunctionContext.getArguments())[0];
		} else {
			invocation = (RemoteMethodInvocation)regionFunctionContext.getArguments();
		}
		
		regionFunctionContext.getFilter();
		
		Object instance = createDelegateInstance(invocation.getClassName(), region);		 

		Serializable result = invokeDelegateMethod(instance,invocation, region);

		if (hasResult()){
			sendResults(regionFunctionContext.getResultSender(),result);
		}

	}
	
	protected final Serializable invokeDelegateMethod(Object instance, RemoteMethodInvocation invocation, Region<?,?> region) {
	
		
		// Null parameters returns any method signature 
		Method method = ReflectionUtils.findMethod(instance.getClass(),invocation.getMethodName(),(Class<?>[])null);
		
		if (method == null ) {
			throw new FunctionException("cannot find method ["+ invocation.getMethodName() + 
					"] on type [" + instance.getClass().getName() + "]");
		}
		
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("about to invoke method %s on class %s", invocation.getMethodName(),invocation.getClassName()));
			for (Object arg: invocation.getArguments()) {
				logger.debug("arg:"+ arg.getClass().getName() + " " + arg.toString());	
			}
			
		}

		return (Serializable)ReflectionUtils.invokeMethod(method, instance, (Object[]) invocation.getArguments());
	}

	

	protected final Object createDelegateInstance(String delegateClassName, Map<?,?> region) {
		Object instance = null;
		try {
			Class<?> clazz = Class.forName(delegateClassName);
			Constructor<?> defaultConstructor = ClassUtils.getConstructorIfAvailable(clazz, new Class<?>[] {});
			if (region != null){
				Constructor<?> mapConstructor = ClassUtils.getConstructorIfAvailable(clazz, Map.class);
				if (mapConstructor != null){
					instance = mapConstructor.newInstance(region);
				} 
			} 
			if (instance == null) {	
				if (defaultConstructor == null) {
					throw new FunctionException("Delegate type [" + delegateClassName + "] has no default constructor");
				}
				instance = defaultConstructor.newInstance();
			}	
		} catch (ClassNotFoundException e) {
		    logger.error(e.getMessage(),e);
			throw new FunctionException("Delegate type [" + delegateClassName + "] not found", e);
		} catch (SecurityException e) {
		    logger.error(e.getMessage(),e);
			throw new FunctionException(e);
		} catch (IllegalArgumentException e) {
		    logger.error(e.getMessage(),e);
			throw new FunctionException("Delegate constructor failed",e);
		} catch (InstantiationException e) {
		    logger.error(e.getMessage(),e);
			throw new FunctionException("Delegate constructor failed",e);
		} catch (IllegalAccessException e) {
		    logger.error(e.getMessage(),e);
			throw new FunctionException("Delegate constructor failed",e);
		} catch (InvocationTargetException e) {
		    logger.error(e.getMessage(),e);
			throw new FunctionException("Delegate constructor failed",e);
		} catch (Throwable t){
		    logger.error(t.getMessage(),t);
            throw new FunctionException("Delegate constructor failed",t);
		}
		return instance;
	}

	/*
	 * @param regionFunctionContext
	 * @return
	 */
	private Region<?, ?> getRegionForContext(RegionFunctionContext regionFunctionContext) {
		
		Region<?,?> region = regionFunctionContext.getDataSet();
		if (PartitionRegionHelper.isPartitionedRegion(region)) {
			if (logger.isDebugEnabled()){
				logger.debug("this is a partitioned region - filtering local data for context");
			}
			region = PartitionRegionHelper.getLocalDataForContext(regionFunctionContext);
		}
		if (logger.isDebugEnabled()){
			logger.debug("region contains " + region.size() + " items");
		}
		return region;
	}

	@SuppressWarnings("unchecked")
	private void sendResults(ResultSender<Object> resultSender, Serializable result) {
		if (result == null){
			resultSender.lastResult(null);
			return;
		}
		
		Serializable lastItem = result;
		
		List<Serializable> results = null;
		if (ObjectUtils.isArray(result)){
			results = Arrays.asList((Serializable[])result);
		} else if (List.class.isAssignableFrom(result.getClass())) {
			results = (List<Serializable>)result;
		}
		
		if (results != null){
			int i = 0;
			for (Serializable item: results){
				if (i++ < results.size() - 1) {
					resultSender.sendResult(item);
				} else {
					lastItem = item;
				}
			}
		}
		resultSender.lastResult(lastItem);		
	}
}

