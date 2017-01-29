/*
 * Copyright 2010-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config.xml;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.util.Properties;

import org.apache.geode.pdx.PdxSerializer;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The ClientCacheNamespaceTest class is a test suite of test cases testing the contract and functionality
 * of the Spring Data GemFire ClientCacheParser.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.6.3
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class ClientCacheNamespaceTest {

	@Autowired
	private ClientCacheFactoryBean clientCacheFactoryBean;

	@Autowired
	private Properties gemfireProperties;

	@Autowired
	private PdxSerializer reflectionPdxSerializer;

	@Test
	public void clientCacheFactoryBeanConfiguration() throws Exception {
		assertThat(clientCacheFactoryBean.getCacheXml().toString(), containsString("empty-client-cache.xml"));
		assertThat(clientCacheFactoryBean.getProperties(), is(equalTo(gemfireProperties)));
		assertThat(clientCacheFactoryBean.getCopyOnRead(), is(true));
		assertThat(clientCacheFactoryBean.getCriticalHeapPercentage(), is(equalTo(0.85f)));
		assertThat(clientCacheFactoryBean.getDurableClientId(), is(equalTo("TestDurableClientId")));
		assertThat(clientCacheFactoryBean.getDurableClientTimeout(), is(equalTo(600)));
		assertThat(clientCacheFactoryBean.getEvictionHeapPercentage(), is(equalTo(0.65f)));
		assertThat(clientCacheFactoryBean.isKeepAlive(), is(true));
		assertThat(clientCacheFactoryBean.getPdxIgnoreUnreadFields(), is(true));
		assertThat(clientCacheFactoryBean.getPdxPersistent(), is(false));
		assertThat(clientCacheFactoryBean.getPdxReadSerialized(), is(true));
		assertThat((PdxSerializer) clientCacheFactoryBean.getPdxSerializer(), is(equalTo(reflectionPdxSerializer)));
		assertThat(TestUtils.<String>readField("poolName", clientCacheFactoryBean), is(equalTo("serverPool")));
		assertThat(clientCacheFactoryBean.getReadyForEvents(), is(false));
	}

}
