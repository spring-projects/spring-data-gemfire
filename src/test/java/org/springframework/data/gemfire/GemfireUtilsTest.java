/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.util.Properties;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.distributed.DistributedSystem;
import org.junit.Test;

/**
 * The GemfireUtilsTest class is a test suite of test cases testing the contract and functionality of the GemfireUtils
 * abstract utility class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.GemfireUtils
 * @since 1.3.3
 */
public class GemfireUtilsTest {

	@Test
	public void isClientWithClientIsTrue() {

		ClientCache mockClient = mock(ClientCache.class);

		assertThat(GemfireUtils.isClient(mockClient), is(true));

		verifyZeroInteractions(mockClient);
	}

	@Test
	public void isClientWithNonClientIsFalse() {

		Cache mockCache = mock(Cache.class);

		assertThat(GemfireUtils.isClient(mockCache), is(false));

		verifyZeroInteractions(mockCache);
	}

	@Test
	public void isDurableWithDurableClientIsTrue() {

		ClientCache mockClientCache = mock(ClientCache.class);

		DistributedSystem mockDistributedSystem = mock(DistributedSystem.class);

		Properties gemfireProperties = new Properties();

		gemfireProperties.setProperty(GemfireUtils.DURABLE_CLIENT_ID_PROPERTY_NAME, "123");

		when(mockClientCache.getDistributedSystem()).thenReturn(mockDistributedSystem);
		when(mockDistributedSystem.isConnected()).thenReturn(true);
		when(mockDistributedSystem.getProperties()).thenReturn(gemfireProperties);

		assertThat(GemfireUtils.isDurable(mockClientCache), is(true));

		verify(mockClientCache, times(1)).getDistributedSystem();
		verify(mockDistributedSystem, times(1)).isConnected();
		verify(mockDistributedSystem, times(1)).getProperties();
	}

	@Test
	public void isDurableWithNonDurableClientIsFalse() {

		ClientCache mockClientCache = mock(ClientCache.class);

		DistributedSystem mockDistributedSystem = mock(DistributedSystem.class);

		Properties gemfireProperties = new Properties();

		gemfireProperties.setProperty(GemfireUtils.DURABLE_CLIENT_ID_PROPERTY_NAME, "  ");

		when(mockClientCache.getDistributedSystem()).thenReturn(mockDistributedSystem);
		when(mockDistributedSystem.isConnected()).thenReturn(true);
		when(mockDistributedSystem.getProperties()).thenReturn(gemfireProperties);

		assertThat(GemfireUtils.isDurable(mockClientCache), is(false));

		verify(mockClientCache, times(1)).getDistributedSystem();
		verify(mockDistributedSystem, times(1)).isConnected();
		verify(mockDistributedSystem, times(1)).getProperties();
	}

	@Test
	public void isDurableWhenDistributedSystemIsNotConnectedIsFalse() {

		ClientCache mockClientCache = mock(ClientCache.class);

		DistributedSystem mockDistributedSystem = mock(DistributedSystem.class);

		when(mockClientCache.getDistributedSystem()).thenReturn(mockDistributedSystem);
		when(mockDistributedSystem.isConnected()).thenReturn(false);

		assertThat(GemfireUtils.isDurable(mockClientCache), is(false));

		verify(mockClientCache, times(1)).getDistributedSystem();
		verify(mockDistributedSystem, times(1)).isConnected();
		verify(mockDistributedSystem, never()).getProperties();
	}

	@Test
	public void isPeerWithPeerIsTrue() {

		Cache mockCache = mock(Cache.class);

		assertThat(GemfireUtils.isPeer(mockCache), is(true));

		verifyZeroInteractions(mockCache);
	}

	@Test
	public void isPeerWithNonPeerIsFalse() {

		ClientCache mockClientCache = mock(ClientCache.class);

		assertThat(GemfireUtils.isPeer(mockClientCache), is(false));

		verifyZeroInteractions(mockClientCache);
	}
}
