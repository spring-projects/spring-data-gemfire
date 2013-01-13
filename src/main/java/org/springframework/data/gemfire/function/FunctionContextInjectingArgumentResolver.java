/*
 * Copyright 2002-2013 the original author or authors.
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

import java.lang.reflect.Method;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.data.gemfire.function.config.Filter;
import org.springframework.data.gemfire.function.config.GemfireFunctionUtils;
import org.springframework.data.gemfire.function.config.RegionData;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.execute.FunctionContext;
import com.gemstone.gemfire.cache.execute.RegionFunctionContext;
import com.gemstone.gemfire.cache.partition.PartitionRegionHelper;
 

/**
 * @author David Turanski
 *
 */
public class FunctionContextInjectingArgumentResolver extends DefaultFunctionArgumentResolver {
	
	private static Log logger = LogFactory.getLog(FunctionContextInjectingArgumentResolver.class);
	
	private final int regionParameterPosition;
	private final int filterParameterPosition;
	private final int functionContextParameterPosition; 
	private final Method method;
	
	public FunctionContextInjectingArgumentResolver(Method method) {
		 
		 this.method = method;
		 int annotatedRegionDataParameterPosition = GemfireFunctionUtils.getAnnotationParameterPosition(method, RegionData.class, new Class[]{Map.class});
		 int regionTypeParameterPosition = getArgumentTypePosition(method,Region.class);
		
		 if (annotatedRegionDataParameterPosition >=0 && regionTypeParameterPosition >= 0) {
			Assert.isTrue(annotatedRegionDataParameterPosition == regionTypeParameterPosition, 
					String.format("Function method signature for method %s cannot contain an @RegionData parameter and a different Region type parameter", method.getName()));
		 }
		 
		 int tempRegionParameterPosition = -1;
					
		 if (annotatedRegionDataParameterPosition >=0 ) {
			tempRegionParameterPosition =  annotatedRegionDataParameterPosition;
		 } else if (regionTypeParameterPosition >=0) {
			 tempRegionParameterPosition = regionTypeParameterPosition;
		 } 
		 
		 regionParameterPosition = tempRegionParameterPosition;                 
		 filterParameterPosition = GemfireFunctionUtils.getAnnotationParameterPosition(method, Filter.class, new Class[]{Set.class});
		 functionContextParameterPosition = getArgumentTypePosition(method,FunctionContext.class);
		 
		
		if (regionParameterPosition >=0  && filterParameterPosition >=0) {
		 	Assert.state(regionParameterPosition != filterParameterPosition, "region parameter and filter parameter must be different");
		} 
		
	 }

	@Override
	public Object[] resolveFunctionArguments(FunctionContext functionContext) {
		
		Object[] args = super.resolveFunctionArguments(functionContext);
		
		if (functionContext instanceof RegionFunctionContext) {
		  if (this.regionParameterPosition >= 0) {
			args =  ArrayUtils.insert(args, regionParameterPosition, getRegionForContext((RegionFunctionContext)functionContext));
		  }
		  
		  if (this.filterParameterPosition >= 0) {
			 args = ArrayUtils.insert(args, filterParameterPosition, ((RegionFunctionContext)functionContext).getFilter());
		  }
		  
		  if (this.functionContextParameterPosition >= 0) {
			args = ArrayUtils.insert(args, functionContextParameterPosition, functionContext);
		  }
		  
		}
		
		Assert.isTrue(args.length == method.getParameterTypes().length, 
				String.format("wrong number of arguments for method %s. Expected :%d, actual: %d", method.getName(),
						method.getParameterTypes().length, args.length));

		
		return args;

	}
	
	/*
	 * @param regionFunctionContext
	 * @return
	 */
	private static Region<?, ?> getRegionForContext(RegionFunctionContext regionFunctionContext) {

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

	private static int getArgumentTypePosition(Method method, Class<?> requiredType) {
 		int position = -1;
 		int i = 0;
		for (Class<?> clazz: method.getParameterTypes()) {
			if (requiredType.equals(clazz)) {
				Assert.state(position < 0, String.format("Method %s signature cannot contain more than one parameter of type %s."
						,method.getName(),requiredType.getName()));
				position = i;
			}
			i++;
		}
		return position;
		
	}
	
	
}
