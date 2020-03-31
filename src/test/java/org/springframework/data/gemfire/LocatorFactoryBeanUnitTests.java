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
package org.springframework.data.gemfire;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.net.InetAddress;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InOrder;
import org.mockito.Mockito;

import org.apache.geode.distributed.Locator;
import org.apache.geode.distributed.LocatorLauncher;

import org.springframework.data.gemfire.config.annotation.LocatorConfigurer;

/**
 * Unit Tests for {@link LocatorFactoryBean}.
 *
 * @author John Blum
 * @see java.util.Properties
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.apache.geode.distributed.Locator
 * @see org.apache.geode.distributed.LocatorLauncher
 * @see org.springframework.data.gemfire.LocatorFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.LocatorConfigurer
 * @since 2.2.0
 */
public class LocatorFactoryBeanUnitTests {

	public LocatorFactoryBean locatorFactoryBean;

	@Before
	public void setup() {

		this.locatorFactoryBean = spy(new LocatorFactoryBean());
	}

	@Test
	public void afterPropertiesSetCallsApplyLocatorConfigurersAndInit() throws Exception {

		doNothing().when(this.locatorFactoryBean).applyLocatorConfigurers(any(LocatorConfigurer.class));
		doNothing().when(this.locatorFactoryBean).init();

		this.locatorFactoryBean.afterPropertiesSet();

		InOrder inOrder = Mockito.inOrder(this.locatorFactoryBean);

		inOrder.verify(this.locatorFactoryBean, times(1)).getCompositeLocatorConfigurer();
		inOrder.verify(this.locatorFactoryBean, times(1)).applyLocatorConfigurers(any(LocatorConfigurer.class));
		inOrder.verify(this.locatorFactoryBean, times(1)).init();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void appliesArrayOfLocatorConfigurersCallsIterableOfLocatorConfigurers() {

		LocatorConfigurer mockLocatorConfigurerOne = mock(LocatorConfigurer.class);
		LocatorConfigurer mockLocatorConfigurerTwo = mock(LocatorConfigurer.class);

		doCallRealMethod().when(this.locatorFactoryBean).applyLocatorConfigurers(any(LocatorConfigurer.class));

		doAnswer(invocation -> {

			Iterable<LocatorConfigurer> locatorConfigurers = invocation.getArgument(0, Iterable.class);

			assertThat(locatorConfigurers).isNotNull();
			assertThat(locatorConfigurers).containsExactly(mockLocatorConfigurerOne, mockLocatorConfigurerTwo);

			return null;

		}).when(this.locatorFactoryBean).applyLocatorConfigurers(any(Iterable.class));

		this.locatorFactoryBean.applyLocatorConfigurers(mockLocatorConfigurerOne, mockLocatorConfigurerTwo);

		verify(this.locatorFactoryBean, times(1)).applyLocatorConfigurers(any(Iterable.class));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void appliesIterableOfLocatorConfigurersInvokesLocatorConfigurers() {

		LocatorConfigurer mockLocatorConfigurerOne = mock(LocatorConfigurer.class);
		LocatorConfigurer mockLocatorConfigurerTwo = mock(LocatorConfigurer.class);

		doCallRealMethod().when(this.locatorFactoryBean).applyLocatorConfigurers(any(Iterable.class));

		this.locatorFactoryBean.setBeanName("TestLocator");
		this.locatorFactoryBean
			.applyLocatorConfigurers(Arrays.asList(mockLocatorConfigurerOne, mockLocatorConfigurerTwo));

		verify(mockLocatorConfigurerOne, times(1))
			.configure(eq("TestLocator"), eq(this.locatorFactoryBean));

		verify(mockLocatorConfigurerTwo, times(1))
			.configure(eq("TestLocator"), eq(this.locatorFactoryBean));
	}

	@Test
	public void initBuildsLocator() throws Exception {

		Locator mockLocator = mock(Locator.class);

		LocatorLauncher mockLocatorLauncher = mock(LocatorLauncher.class);

		LocatorLauncher.Builder locatorBuilderSpy = spy(new LocatorLauncher.Builder());

		when(mockLocatorLauncher.getLocator()).thenReturn(mockLocator);
		when(locatorBuilderSpy.build()).thenReturn(mockLocatorLauncher);
		when(this.locatorFactoryBean.newLocatorLauncherBuilder()).thenReturn(locatorBuilderSpy);

		String testBindAddress = InetAddress.getLocalHost().getHostAddress();

		this.locatorFactoryBean.setBeanName("TestLocatorBean");
		this.locatorFactoryBean.setBindAddress(testBindAddress);
		this.locatorFactoryBean.setHostnameForClients("skullbox");
		this.locatorFactoryBean.setLocators("host1[1234],host2[6789]");
		this.locatorFactoryBean.setName("TestMember");
		this.locatorFactoryBean.setPort(54321);
		this.locatorFactoryBean.init();

		assertThat(this.locatorFactoryBean.getLocator()).isEqualTo(mockLocator);
		assertThat(this.locatorFactoryBean.getLocatorLauncher()).isEqualTo(mockLocatorLauncher);

		verify(locatorBuilderSpy, times(1)).set(eq("locators"), eq("host1[1234],host2[6789]"));
		verify(locatorBuilderSpy, times(1)).set(eq("log-level"), eq("config"));
		verify(locatorBuilderSpy, times(1)).setBindAddress(eq(testBindAddress));
		verify(locatorBuilderSpy, times(1)).setHostnameForClients(eq("skullbox"));
		verify(locatorBuilderSpy, times(1)).setMemberName(eq("TestMember"));
		verify(locatorBuilderSpy, times(1)).setPort(eq(54321));
		verify(this.locatorFactoryBean, times(1)).postProcess(eq(locatorBuilderSpy));
		verify(this.locatorFactoryBean, times(1)).postProcess(eq(mockLocatorLauncher));
		verify(mockLocatorLauncher, times(1)).start();
	}

	@Test
	public void configuresLocatorLauncherBuilderGemFireProperties() {

		Properties gemfireProperties = new Properties();

		gemfireProperties.setProperty("name", "TEST");
		gemfireProperties.setProperty("log-level", "config");
		gemfireProperties.setProperty("locators", "localhost[11235],skullbox[12480]");

		LocatorLauncher.Builder locatorBuilderSpy = spy(new LocatorLauncher.Builder());

		this.locatorFactoryBean.setGemFireProperties(gemfireProperties);
		this.locatorFactoryBean.configureGemfireProperties(locatorBuilderSpy);

		gemfireProperties.stringPropertyNames().forEach(propertyName ->
			verify(locatorBuilderSpy, times(1))
				.set(eq(propertyName), eq(gemfireProperties.getProperty(propertyName))));
	}

	@Test
	public void getObjectReturnsLocator() throws Exception {

		Locator mockLocator = mock(Locator.class);

		doReturn(mockLocator).when(this.locatorFactoryBean).getLocator();

		assertThat(this.locatorFactoryBean.getObject()).isEqualTo(mockLocator);

		verify(this.locatorFactoryBean, times(1)).getLocator();
	}

	@Test(expected = IllegalStateException.class)
	public void getObjectThrowsIllegalStateException() throws Exception {

		try {
			this.locatorFactoryBean.getObject();
		}
		catch (IllegalStateException expected) {

			assertThat(expected).hasMessage("Locator was not configured and initialized");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void getObjectTypeEqualsLocatorClass() {

		assertThat(this.locatorFactoryBean.getObjectType()).isEqualTo(Locator.class);

		verify(this.locatorFactoryBean, times(1)).getLocator();
	}

	@Test
	public void getObjectTypeIsALocatorType() {

		Locator mockLocator = mock(Locator.class);

		doReturn(mockLocator).when(this.locatorFactoryBean).getLocator();

		Class<?> objectType = this.locatorFactoryBean.getObjectType();

		assertThat(Locator.class).isAssignableFrom(objectType);
		assertThat(objectType).isEqualTo(mockLocator.getClass());

		verify(this.locatorFactoryBean, times(1)).getLocator();
	}

	@Test
	public void setAndGetBindAddress() {

		this.locatorFactoryBean.setBindAddress("10.105.210.16");

		assertThat(this.locatorFactoryBean.getBindAddress().orElse(null)).isEqualTo("10.105.210.16");

		this.locatorFactoryBean.setBindAddress(null);

		assertThat(this.locatorFactoryBean.getBindAddress().orElse(null)).isNull();

		this.locatorFactoryBean.setBindAddress("  ");

		assertThat(this.locatorFactoryBean.getBindAddress().orElse(null)).isNull();
	}

	@Test
	public void setAndGetGemFireProperties() {

		Properties testGemFireProperties = new Properties();

		testGemFireProperties.setProperty("testKey", "testValue");

		this.locatorFactoryBean.setGemFireProperties(testGemFireProperties);

		assertThat(this.locatorFactoryBean.getGemFireProperties()).isEqualTo(testGemFireProperties);

		this.locatorFactoryBean.setGemFireProperties(null);

		Properties actualGemFireProperties = this.locatorFactoryBean.getGemFireProperties();

		assertThat(actualGemFireProperties).isNotNull();
		assertThat(actualGemFireProperties).isNotEqualTo(testGemFireProperties);
		assertThat(actualGemFireProperties).isEmpty();
	}

	@Test
	public void setAndGetHostnameForClients() {

		this.locatorFactoryBean.setHostnameForClients("skullbox");

		assertThat(this.locatorFactoryBean.getHostnameForClients().orElse(null)).isEqualTo("skullbox");

		this.locatorFactoryBean.setHostnameForClients(null);

		assertThat(this.locatorFactoryBean.getHostnameForClients().orElse(null)).isNull();

		this.locatorFactoryBean.setHostnameForClients("  ");

		assertThat(this.locatorFactoryBean.getHostnameForClients().orElse(null)).isNull();
	}

	@Test
	public void setArrayOfLocatorConfigurersCallsListOfLocatorConfigurers() {

		LocatorConfigurer mockLocatorConfigurerOne = mock(LocatorConfigurer.class);
		LocatorConfigurer mockLocatorConfigurerTwo = mock(LocatorConfigurer.class);

		this.locatorFactoryBean.setLocatorConfigurers(mockLocatorConfigurerOne, mockLocatorConfigurerTwo);

		List<LocatorConfigurer> locatorconfigurers = Arrays.asList(mockLocatorConfigurerOne, mockLocatorConfigurerTwo);

		verify(this.locatorFactoryBean, times(1)).setLocatorConfigurers(eq(locatorconfigurers));
	}

	@Test
	public void setListOfLocatorConfigurersAddsAll() {

		LocatorConfigurer mockLocatorConfigurerOne = mock(LocatorConfigurer.class);
		LocatorConfigurer mockLocatorConfigurerTwo = mock(LocatorConfigurer.class);

		List<LocatorConfigurer> locatorconfigurers = Arrays.asList(mockLocatorConfigurerOne, mockLocatorConfigurerTwo);

		this.locatorFactoryBean.setLocatorConfigurers(locatorconfigurers);

		LocatorConfigurer compositeLocatorConfigurer = this.locatorFactoryBean.getCompositeLocatorConfigurer();

		assertThat(compositeLocatorConfigurer).isNotNull();

		compositeLocatorConfigurer.configure("TestLocator", this.locatorFactoryBean);

		verify(mockLocatorConfigurerOne, times(1))
			.configure(eq("TestLocator"), eq(this.locatorFactoryBean));

		verify(mockLocatorConfigurerTwo, times(1))
			.configure(eq("TestLocator"), eq(this.locatorFactoryBean));
	}

	@Test
	public void setAndGetLocators() {

		this.locatorFactoryBean.setLocators("skullbox[11235]");

		assertThat(this.locatorFactoryBean.getLocators().orElse(null)).isEqualTo("skullbox[11235]");

		this.locatorFactoryBean.setLocators(null);

		assertThat(this.locatorFactoryBean.getLocators().orElse(null)).isNull();

		this.locatorFactoryBean.setLocators("");

		assertThat(this.locatorFactoryBean.getLocators().orElse(null)).isNull();

		this.locatorFactoryBean.setLocators("  ");

		assertThat(this.locatorFactoryBean.getLocators().orElse(null)).isNull();

		this.locatorFactoryBean.setLocators("host1[1234],host2[6789]");

		assertThat(this.locatorFactoryBean.getLocators().orElse(null)).isEqualTo("host1[1234],host2[6789]");
	}

	@Test
	public void setAndGetLogLevel() {

		this.locatorFactoryBean.setLogLevel("info");

		assertThat(this.locatorFactoryBean.getLogLevel()).isEqualTo("info");

		this.locatorFactoryBean.setLogLevel(null);

		assertThat(this.locatorFactoryBean.getLogLevel()).isEqualTo(LocatorFactoryBean.DEFAULT_LOG_LEVEL);

		this.locatorFactoryBean.setLogLevel("  ");

		assertThat(this.locatorFactoryBean.getLogLevel()).isEqualTo(LocatorFactoryBean.DEFAULT_LOG_LEVEL);
	}

	@Test
	public void setAndGetName() {

		this.locatorFactoryBean.setName("TestLocator");

		assertThat(this.locatorFactoryBean.getName().orElse(null)).isEqualTo("TestLocator");

		this.locatorFactoryBean.setName(null);

		assertThat(this.locatorFactoryBean.getName().orElse(null)).isNull();

		this.locatorFactoryBean.setName("  ");

		assertThat(this.locatorFactoryBean.getName().orElse(null)).isNull();
	}

	@Test
	public void setPortToValidValue() {

		this.locatorFactoryBean.setPort(54321);

		assertThat(this.locatorFactoryBean.getPort()).isEqualTo(54321);
	}

	@Test(expected = IllegalArgumentException.class)
	public void setPortToOverflowValue() {

		try {
			this.locatorFactoryBean.setPort(65536);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("Network port [65536] is not valid");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void setPortToUnderflowValue() {

		try {
			this.locatorFactoryBean.setPort(-1);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("Network port [-1] is not valid");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}
}
