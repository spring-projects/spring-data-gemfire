/*
 * Copyright 2002-2019 the original author or authors.
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

import org.apache.geode.cache.Region;
import org.apache.geode.cache.execute.Function;
import org.apache.geode.cache.execute.FunctionContext;
import org.apache.geode.cache.execute.RegionFunctionContext;
import org.apache.geode.cache.execute.ResultSender;
import org.apache.geode.cache.partition.PartitionRegionHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.gemfire.function.annotation.Filter;
import org.springframework.data.gemfire.function.annotation.RegionData;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.util.Assert;

/**
 * {@link FunctionArgumentResolver} implementation capable of resolving the {@link FunctionContext} passed to
 * a {@link Function} implementation during invocation.
 *
 * @author David Turanski
 * @author John Blum
 * @see java.lang.reflect.Method
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.execute.Function
 * @see org.apache.geode.cache.execute.FunctionContext
 * @see org.apache.geode.cache.execute.RegionFunctionContext
 * @see org.apache.geode.cache.execute.ResultSender
 * @see org.apache.geode.cache.partition.PartitionRegionHelper
 * @see org.springframework.data.gemfire.function.annotation.Filter
 * @see org.springframework.data.gemfire.function.annotation.RegionData
 * @see org.springframework.data.gemfire.function.PdxFunctionArgumentResolver
 * @since 1.3.0
 */
class FunctionContextInjectingArgumentResolver extends PdxFunctionArgumentResolver {

	private final Logger logger = LoggerFactory.getLogger(FunctionContextInjectingArgumentResolver.class);

	private final int filterParameterPosition;
	private final int functionContextParameterPosition;
	private final int regionParameterPosition;
	private final int resultSenderParameterPosition;

	private final Method method;

	FunctionContextInjectingArgumentResolver(Method method) {

		this.method = method;

		int regionDataAnnotationParameterPosition = GemfireFunctionUtils.getAnnotationParameterPosition(
			method, RegionData.class, new Class[] { Map.class });

		int regionTypeParameterPosition = getArgumentTypePosition(method, Region.class);

		if (regionDataAnnotationParameterPosition >= 0 && regionTypeParameterPosition >= 0) {
			Assert.isTrue(regionDataAnnotationParameterPosition == regionTypeParameterPosition, String.format(
				"Function method signature for method %s cannot contain an @RegionData parameter and a different Region type parameter",
					method.getName()));
		}

		regionParameterPosition = (regionDataAnnotationParameterPosition >= 0 ? regionDataAnnotationParameterPosition
			: (regionTypeParameterPosition >= 0 ? regionTypeParameterPosition : -1));

		filterParameterPosition = GemfireFunctionUtils.getAnnotationParameterPosition(method, Filter.class,
			new Class[] { Set.class });

		if (regionParameterPosition >= 0 && filterParameterPosition >= 0) {
			Assert.state(regionParameterPosition != filterParameterPosition,
				"region parameter and filter parameter must be different");
		}

		functionContextParameterPosition = getArgumentTypePosition(method, FunctionContext.class);

		resultSenderParameterPosition = getArgumentTypePosition(method, ResultSender.class);
	}

	@Override
	public Method getFunctionAnnotatedMethod() {
		return this.method;
	}

	@Override
	public Object[] resolveFunctionArguments(FunctionContext functionContext) {

		Object[] args = super.resolveFunctionArguments(functionContext);

		if (functionContext instanceof RegionFunctionContext) {
			if (this.regionParameterPosition >= 0) {
				args = ArrayUtils.insert(args, regionParameterPosition, getRegionForContext(
					(RegionFunctionContext) functionContext));
			}

			if (this.filterParameterPosition >= 0) {
				args = ArrayUtils.insert(args, filterParameterPosition,
					((RegionFunctionContext) functionContext).getFilter());
			}
		}

		if (this.functionContextParameterPosition >= 0) {
			args = ArrayUtils.insert(args, functionContextParameterPosition, functionContext);
		}

		if (this.resultSenderParameterPosition >= 0) {
			args = ArrayUtils.insert(args, resultSenderParameterPosition, functionContext.getResultSender());
		}

		Assert.isTrue(args.length == this.method.getParameterTypes().length,
			String.format("Wrong number of arguments for method [%s]; Expected [%d], but was [%d]",
				this.method.getName(), this.method.getParameterTypes().length, args.length));

		return args;
	}

	private Region<?, ?> getRegionForContext(RegionFunctionContext regionFunctionContext) {

		Region<?, ?> region = regionFunctionContext.getDataSet();

		if (PartitionRegionHelper.isPartitionedRegion(region)) {

			if (logger.isDebugEnabled()) {
				logger.debug("this is a partitioned region - filtering local data for context");
			}

			region = PartitionRegionHelper.getLocalDataForContext(regionFunctionContext);
		}

		if (logger.isDebugEnabled()) {
			logger.debug("Region contains {} items", region.size());
		}

		return region;
	}

	int getArgumentTypePosition(Method method, Class<?> requiredType) {

		int index = 0;
		int position = -1;

		for (Class<?> parameterType : method.getParameterTypes()) {
			if (requiredType.isAssignableFrom(parameterType)) {

				Assert.state(position < 0,
					String.format("Method [%1$s] signature cannot contain more than one parameter of type [%2$s].",
						method.getName(), requiredType.getName()));

				position = index;
			}

			index++;
		}

		return position;
	}
}
