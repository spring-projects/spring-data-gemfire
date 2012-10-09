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
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.util.ClassUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.execute.Function;
import com.gemstone.gemfire.cache.execute.FunctionContext;
import com.gemstone.gemfire.cache.execute.RegionFunctionContext;
import com.gemstone.gemfire.cache.execute.ResultSender;
import com.gemstone.gemfire.cache.partition.PartitionRegionHelper;
import com.gemstone.gemfire.internal.util.ArrayUtils;

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
public class PojoFunctionWrapper implements Function {

	private static transient Log logger = LogFactory.getLog(PojoFunctionWrapper.class);

	private volatile boolean HA;
	private volatile boolean optimizeForWrite;
	private final boolean hasResult;
	private final Object target;
	private final Method method;
	private final String id;
	private volatile Integer regionParameterPosition;

	public PojoFunctionWrapper(Object target, Method method, String id) {
		this.id = StringUtils.hasText(id) ? id : ClassUtils.getQualifiedMethodName(method);
		this.target = target;
		this.method = method;

		this.HA = false;
		
		this.hasResult = !(method.getReturnType().equals(void.class));
				
		this.optimizeForWrite = false;
	}

	//@Override	 
	public String getId() {
		return this.id;
	}

	//@Override	 
	public boolean hasResult() {
		return this.hasResult;
	}

	//@Override	
	public boolean isHA() {
		return this.HA;
	}

	public void setHA(boolean HA) {
		this.HA = HA;
	}

	//@Override	
	public boolean optimizeForWrite() {
		return this.optimizeForWrite;
	}

	public void setOptimizeForWrite(boolean optimizeForWrite) {
		this.optimizeForWrite = optimizeForWrite;
	}
	
	public void setRegionParameterPosition(int regionParameterPosition) {
		this.regionParameterPosition = regionParameterPosition; 
	}

	//@Override
	public void execute(FunctionContext functionContext) {

		Object[] args = (functionContext.getArguments().getClass().isArray()) ? (Object[]) functionContext
				.getArguments() : new Object[] { functionContext.getArguments() };

		Serializable result = null;

		if (functionContext instanceof RegionFunctionContext) {
			RegionFunctionContext regionFunctionContext = (RegionFunctionContext) functionContext;
			Region<?, ?> region = getRegionForContext(regionFunctionContext);
			//TODO: Not sure if filter is needed at this point
			Set<?> filter = regionFunctionContext.getFilter();
			//Insert the region into the associated position
			if (this.regionParameterPosition != null) {
				ArrayUtils.insert(args, regionParameterPosition, region);
			}
	
		} else {
			
		}

		result = invokeTargetMethod(args);
		
		if (hasResult()) {
			sendResults(functionContext.getResultSender(), result);
		}
	}

 
	protected final Serializable invokeTargetMethod(Object[] args) {

		if (logger.isDebugEnabled()) {
			logger.debug(String.format("about to invoke method %s on class %s as function %s", method.getName(), target
					.getClass().getName(), this.id));

			for (Object arg : args) {
				logger.debug("arg:" + arg.getClass().getName() + " " + arg.toString());
			}

		}

		return (Serializable) ReflectionUtils.invokeMethod(method, target, (Object[]) args);
	}

	/*
	 * @param regionFunctionContext
	 * @return
	 */
	private Region<?, ?> getRegionForContext(RegionFunctionContext regionFunctionContext) {

		Region<?, ?> region = regionFunctionContext.getDataSet();
		if (PartitionRegionHelper.isPartitionedRegion(region)) {
			if (logger.isDebugEnabled()) {
				logger.debug("this is a partitioned region - filtering local data for context");
			}
			region = PartitionRegionHelper.getLocalDataForContext(regionFunctionContext);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("region contains " + region.size() + " items");
		}
		return region;
	}

	@SuppressWarnings("unchecked")
	private void sendResults(ResultSender<Object> resultSender, Serializable result) {
		if (result == null) {
			resultSender.lastResult(null);
			return;
		}

		Serializable lastItem = result;

		List<Serializable> results = null;
		if (ObjectUtils.isArray(result)) {
			results = Arrays.asList((Serializable[]) result);
		} else if (List.class.isAssignableFrom(result.getClass())) {
			results = (List<Serializable>) result;
		}

		if (results != null) {
			int i = 0;
			for (Serializable item : results) {
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
