/*
 * Copyright 2018-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.config.xml;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;

import javax.annotation.Resource;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.asyncqueue.AsyncEventListener;

import org.springframework.data.gemfire.support.AbstractFactoryBeanSupport;
import org.springframework.lang.Nullable;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * The LookupRegionWithAsyncEventQueuesAndGatewaySendersIntegrationTests class...
 *
 * @author John Blum
 * @since 1.0.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class LookupRegionWithAsyncEventQueuesAndGatewaySendersIntegrationTests {

	@Resource(name = "Example")
	private Region<?, ?> example;

	@Test
	public void regionWithAsyncEventQueuesAndGatewaySenderConfigurationIsCorrect() {

		assertThat(this.example).isNotNull();
		assertThat(this.example.getName()).isEqualTo("Example");

		RegionAttributes<?, ?> exampleAttributes = this.example.getAttributes();

		assertThat(exampleAttributes).isNotNull();
		assertThat(exampleAttributes.getDataPolicy()).isEqualTo(DataPolicy.REPLICATE);

		assertThat(exampleAttributes.getAsyncEventQueueIds()).containsExactlyInAnyOrder(
			"TestAsyncEventQueueZero",
			"TestAsyncEventQueueOne",
			"TestAsyncEventQueueTwo",
			"TestAsyncEventQueueThree",
			"TestAsyncEventQueueFour"
		);

		assertThat(exampleAttributes.getGatewaySenderIds()).containsExactlyInAnyOrder(
			"TestGatewaySenderZero",
			"TestGatewaySenderOne",
			"TestGatewaySenderTwo",
			"TestGatewaySenderThree",
			"TestGatewaySenderFour"
		);
	}

	public static final class AsyncEventListenerFactoryBean extends AbstractFactoryBeanSupport<AsyncEventListener> {

		private final AsyncEventListener mockAsyncEventListener = mock(AsyncEventListener.class);

		@Nullable @Override
		public AsyncEventListener getObject() {
			return this.mockAsyncEventListener;
		}

		@Nullable @Override
		public Class<?> getObjectType() {

			return this.mockAsyncEventListener != null
				? this.mockAsyncEventListener.getClass()
				: AsyncEventListener.class;
		}
	}
}
