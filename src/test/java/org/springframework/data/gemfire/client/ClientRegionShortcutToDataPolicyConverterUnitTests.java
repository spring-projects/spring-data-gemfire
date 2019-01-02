/*
 * Copyright 2017-2019 the original author or authors.
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

package org.springframework.data.gemfire.client;

import static org.assertj.core.api.Assertions.assertThat;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.junit.Test;

/**
 * Unit tests for {@link ClientRegionShortcutToDataPolicyConverter}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.DataPolicy
 * @see org.apache.geode.cache.client.ClientRegionShortcut
 * @see org.springframework.data.gemfire.client.ClientRegionShortcutToDataPolicyConverter
 * @since 2.0.2
 */
public class ClientRegionShortcutToDataPolicyConverterUnitTests {

	protected void assertDataPolicy(DataPolicy actual, DataPolicy expected) {
		assertThat(actual).isEqualTo(expected);
	}

	protected void assertDataPolicyDefault(DataPolicy actual) {
		assertDataPolicy(actual, DataPolicy.DEFAULT);
	}

	protected void assertDataPolicyEmpty(DataPolicy actual) {
		assertDataPolicy(actual, DataPolicy.EMPTY);
	}

	protected void assertDataPolicyNormal(DataPolicy actual) {
		assertDataPolicy(actual, DataPolicy.NORMAL);
	}

	protected void assertDataPolicyPersistentReplicate(DataPolicy actual) {
		assertDataPolicy(actual, DataPolicy.PERSISTENT_REPLICATE);
	}

	protected DataPolicy convert(ClientRegionShortcut clientRegionShortcut) {
		return ClientRegionShortcutToDataPolicyConverter.INSTANCE.convert(clientRegionShortcut);
	}

	@Test
	public void clientRegionShortcutCachingProxyIsDataPolicyNormal() {
		assertDataPolicyNormal(convert(ClientRegionShortcut.CACHING_PROXY));
	}

	@Test
	public void clientRegionShortcutCachingProxyHeapLruIsDataPolicyNormal() {
		assertDataPolicyNormal(convert(ClientRegionShortcut.CACHING_PROXY_HEAP_LRU));
	}

	@Test
	public void clientRegionShortcutCachingProxyOverflowIsDataPolicyNormal() {
		assertDataPolicyNormal(convert(ClientRegionShortcut.CACHING_PROXY_OVERFLOW));
	}

	@Test
	public void clientRegionShortcutLocalIsDataPolicyNormal() {
		assertDataPolicyNormal(convert(ClientRegionShortcut.LOCAL));
	}

	@Test
	public void clientRegionShortcutLocalHeapLruIsDataPolicyNormal() {
		assertDataPolicyNormal(convert(ClientRegionShortcut.LOCAL_HEAP_LRU));
	}

	@Test
	public void clientRegionShortcutLocalOverflowIsDataPolicyNormal() {
		assertDataPolicyNormal(convert(ClientRegionShortcut.LOCAL_OVERFLOW));
	}

	@Test
	public void clientRegionShortcutLocalPersistentIsDataPolicyPersistentReplicate() {
		assertDataPolicyPersistentReplicate(convert(ClientRegionShortcut.LOCAL_PERSISTENT));
	}

	@Test
	public void clientRegionShortcutLocalPersistentOverflowIsDataPolicyPersistentReplicate() {
		assertDataPolicyPersistentReplicate(convert(ClientRegionShortcut.LOCAL_PERSISTENT_OVERFLOW));
	}

	@Test
	public void clientRegionShortcutLocalProxyIsDataPolicyEmpty() {
		assertDataPolicyEmpty(convert(ClientRegionShortcut.PROXY));
	}

	@Test
	public void nullClientRegionShortcutIsDataPolicyDefault() {
		assertDataPolicyDefault(convert(null));
	}
}
