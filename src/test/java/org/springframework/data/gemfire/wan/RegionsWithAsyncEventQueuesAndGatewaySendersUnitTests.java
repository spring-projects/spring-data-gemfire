/*
 * Copyright 2018 the original author or authors.
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
package org.springframework.data.gemfire.wan;

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
 * The RegionsWithAsyncEventQueuesAndGatewaySendersUnitTests class...
 *
 * @author John Blum
 * @since 1.0.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class RegionsWithAsyncEventQueuesAndGatewaySendersUnitTests {

	@Resource(name = "TemplateBasedLocalRegion")
	private Region<?, ?> templateBasedLocalRegion;

	@Resource(name = "LocalRegion")
	private Region<?, ?> localRegion;

	@Resource(name = "PartitionRegion")
	private Region<?, ?> partitionRegion;

	@Resource(name = "ReplicateRegion")
	private Region<?, ?> replicateRegion;

	private void assertRegion(Region<?, ?> region, String name, DataPolicy dataPolicy) {

		assertThat(region).isNotNull();
		assertThat(region.getName()).isEqualTo(name);

		RegionAttributes<?, ?> regionAttributes = region.getAttributes();

		assertThat(regionAttributes).isNotNull();
		assertThat(regionAttributes.getDataPolicy()).isEqualTo(dataPolicy);
	}

	@Test
	public void templateBasedlocalRegionConfigurationIsCorrect() {

		assertRegion(this.templateBasedLocalRegion, "TemplateBasedLocalRegion", DataPolicy.NORMAL);

		assertThat(this.templateBasedLocalRegion.getAttributes().getAsyncEventQueueIds())
			.containsExactlyInAnyOrder("X", "Y", "Z");

		assertThat(this.templateBasedLocalRegion.getAttributes().getGatewaySenderIds())
			.containsExactlyInAnyOrder("99", "100", "101");
	}

	@Test
	public void localRegionConfigurationIsCorrect() {

		assertRegion(this.localRegion, "LocalRegion", DataPolicy.NORMAL);

		assertThat(this.localRegion.getAttributes().getAsyncEventQueueIds())
			.containsExactlyInAnyOrder("A", "B", "C", "D", "E");

		assertThat(this.localRegion.getAttributes().getGatewaySenderIds())
			.containsExactlyInAnyOrder("1", "2", "3", "4", "5");
	}

	@Test
	public void partitionRegionConfigurationIsCorrect() {

		assertRegion(this.partitionRegion, "PartitionRegion", DataPolicy.PARTITION);

		assertThat(this.partitionRegion.getAttributes().getAsyncEventQueueIds())
			.containsExactlyInAnyOrder("E", "F", "G", "H", "I");

		assertThat(this.partitionRegion.getAttributes().getGatewaySenderIds())
			.containsExactlyInAnyOrder("5", "6", "7", "8", "9");
	}

	@Test
	public void sreplicateRegionConfigurationIsCorrect() {

		assertRegion(this.replicateRegion, "ReplicateRegion", DataPolicy.REPLICATE);

		assertThat(this.replicateRegion.getAttributes().getAsyncEventQueueIds())
			.containsExactlyInAnyOrder("E", "J", "K", "L", "M");

		assertThat(this.replicateRegion.getAttributes().getGatewaySenderIds())
			.containsExactlyInAnyOrder("5", "10", "11", "12", "13");
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
