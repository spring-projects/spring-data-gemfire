/*
 * Copyright 2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.function;

import static org.assertj.core.api.Assertions.assertThat;

import java.lang.reflect.Method;

import org.apache.geode.cache.execute.FunctionContext;
import org.apache.geode.cache.execute.RegionFunctionContext;
import org.junit.Test;
import org.springframework.data.gemfire.function.annotation.GemfireFunction;
import org.springframework.data.gemfire.util.ArrayUtils;

/**
 * Unit tests for {@link FunctionContextInjectingArgumentResolver}.
 *
 * @author John Blum
 * @see java.lang.reflect.Method
 * @see org.junit.Test
 * @see org.apache.geode.cache.execute.FunctionContext
 * @see org.apache.geode.cache.execute.RegionFunctionContext
 * @see org.springframework.data.gemfire.function.FunctionContextInjectingArgumentResolver
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class FunctionContextInjectingArgumentResolverUnitTests {

	@Test
	public void getArgumentTypePositionWithMethodHavingNoArgumentsReturnsMinusOne() throws Exception {

		Method functionOne = FunctionOne.class.getDeclaredMethod("functionOne");

		FunctionContextInjectingArgumentResolver functionArgumentResolver =
			new FunctionContextInjectingArgumentResolver(functionOne);

		assertThat(functionArgumentResolver.getArgumentTypePosition(functionOne, FunctionContext.class)).isEqualTo(-1);
	}

	@Test
	public void getArgumentTypePositionForExactArgumentTypeReturnsNonNegativeIndex() throws Exception {

		Method functionTwo = FunctionTwo.class
			.getDeclaredMethod("functionTwo", ArrayUtils.asArray(FunctionContext.class));

		FunctionContextInjectingArgumentResolver functionArgumentResolver =
			new FunctionContextInjectingArgumentResolver(functionTwo);

		assertThat(functionArgumentResolver.getArgumentTypePosition(functionTwo, FunctionContext.class)).isEqualTo(0);
	}

	@Test
	public void getArgumentTypePositionForInexactArgumentTypeReturnsNonNegativeIndex() throws Exception {

		Method functionThree = FunctionThree.class
			.getDeclaredMethod("functionThree", ArrayUtils.asArray(Object.class, RegionFunctionContext.class));

		FunctionContextInjectingArgumentResolver functionArgumentResolver =
			new FunctionContextInjectingArgumentResolver(functionThree);

		assertThat(functionArgumentResolver.getArgumentTypePosition(functionThree, FunctionContext.class)).isEqualTo(1);
	}

	@Test
	public void getArgumentTypePositionForNonMatchingArgumentTypeReturnsMinusOne() throws Exception {

		Method functionFour = FunctionFour.class
			.getDeclaredMethod("functionFour", ArrayUtils.asArray(Object.class, String.class));

		FunctionContextInjectingArgumentResolver functionArgumentResolver =
			new FunctionContextInjectingArgumentResolver(functionFour);

		assertThat(functionArgumentResolver.getArgumentTypePosition(functionFour, FunctionContext.class)).isEqualTo(-1);
	}

	static class FunctionOne {

		@GemfireFunction
		void functionOne() { }

	}

	static class FunctionTwo {

		@GemfireFunction
		void functionTwo(FunctionContext functionContext) { }

	}

	static class FunctionThree {

		@GemfireFunction
		void functionThree(Object arg, RegionFunctionContext functionContext) { }
	}

	static class FunctionFour {

		@GemfireFunction
		void functionFour(Object functionContext, String arg) { }

	}
}
