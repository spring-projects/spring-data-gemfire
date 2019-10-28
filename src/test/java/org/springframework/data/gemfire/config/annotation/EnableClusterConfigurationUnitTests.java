/*
 * Copyright 2017-2019 the original author or authors.
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
package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.config.admin.remote.RestHttpGemfireAdminTemplate.FollowRedirectsSimpleClientHttpRequestFactory;
import static org.springframework.data.gemfire.config.annotation.ClusterConfigurationConfiguration.ClusterSchemaObjectInitializer;
import static org.springframework.data.gemfire.config.annotation.ClusterConfigurationConfiguration.SchemaObjectContext;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.junit.After;
import org.junit.Test;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.RegionShortcut;
import org.apache.geode.cache.client.ClientCache;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.ListableBeanFactory;
import org.springframework.core.env.Environment;
import org.springframework.core.env.StandardEnvironment;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.config.admin.GemfireAdminOperations;
import org.springframework.data.gemfire.config.admin.remote.FunctionGemfireAdminTemplate;
import org.springframework.data.gemfire.config.admin.remote.RestHttpGemfireAdminTemplate;
import org.springframework.data.gemfire.config.schema.support.ComposableSchemaObjectCollector;
import org.springframework.data.gemfire.config.schema.support.ComposableSchemaObjectDefiner;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.InterceptingClientHttpRequestFactory;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.client.RestTemplate;

/**
 * Unit Tests for {@link EnableClusterConfiguration} annotation and the {@link ClusterConfigurationConfiguration} class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.core.env.Environment
 * @see org.springframework.core.type.AnnotationMetadata
 * @see org.springframework.data.gemfire.config.admin.GemfireAdminOperations
 * @see org.springframework.data.gemfire.config.annotation.ClusterConfigurationConfiguration
 * @see org.springframework.data.gemfire.config.annotation.EnableClusterConfiguration
 * @see org.springframework.web.client.RestTemplate
 * @since 2.0.1
 */
public class EnableClusterConfigurationUnitTests {

	@After
	public void tearDown() {
		System.clearProperty(ClusterConfigurationConfiguration.HTTP_FOLLOW_REDIRECTS_PROPERTY);
	}

	// TODO: Replace with STDG!
	private <T> ClusterConfigurationConfiguration autowire(ClusterConfigurationConfiguration target,
			String fieldName, T dependency) throws NoSuchFieldException {

		return Optional.ofNullable(ReflectionUtils.findField(target.getClass(), fieldName))
			.map(field -> {
				ReflectionUtils.makeAccessible(field);
				return field;
			})
			.map(field -> {
				ReflectionUtils.setField(field, target, dependency);
				return target;
			})
			.orElseThrow(() ->
				new NoSuchFieldException(String.format("Field [%s] was not found on Object of type [%s]",
					fieldName, target.getClass().getName())));
	}

	@SuppressWarnings("unchecked")
	private <T> T getFieldValue(Object target, String fieldName) throws NoSuchFieldException {

		Field field = ReflectionUtils.findField(target.getClass(), fieldName);

		return Optional.ofNullable(field)
			.map(it -> {
				ReflectionUtils.makeAccessible(it);
				return field;
			})
			.map(it -> (T) ReflectionUtils.getField(it, target))
			.orElseThrow(() ->
				new NoSuchFieldException(String.format("Field with name [%s] was not found on Object of type [%s]",
					fieldName, target.getClass().getName())));
	}

	@Test
	public void setImportMetadataFromAnnotationAttributes() {

		AnnotationMetadata mockImportMetadata = mock(AnnotationMetadata.class);

		Map<String, Object> annotationAttributes = new HashMap<>();

		annotationAttributes.put("host", "skullbox");
		annotationAttributes.put("port", 12345);
		annotationAttributes.put("enableInterceptors", true);
		annotationAttributes.put("followRedirects", true);
		annotationAttributes.put("requireHttps", false);
		annotationAttributes.put("serverRegionShortcut", RegionShortcut.PARTITION_PERSISTENT);
		annotationAttributes.put("useHttp", true);

		when(mockImportMetadata.getAnnotationAttributes(EnableClusterConfiguration.class.getName()))
			.thenReturn(annotationAttributes);

		when(mockImportMetadata.hasAnnotation(eq(EnableClusterConfiguration.class.getName()))).thenReturn(true);

		ClusterConfigurationConfiguration configuration = new ClusterConfigurationConfiguration();

		configuration.setImportMetadata(mockImportMetadata);

		assertThat(configuration.getManagementHttpHost().orElse(null)).isEqualTo("skullbox");
		assertThat(configuration.getManagementHttpPort().orElse(0)).isEqualTo(12345);
		assertThat(configuration.getManagementHttpEnableInterceptors().orElse(false)).isTrue();
		assertThat(configuration.getManagementHttpFollowRedirects().orElse(false)).isTrue();
		assertThat(configuration.getManagementRequireHttps().orElse(true)).isFalse();
		assertThat(configuration.getManagementUseHttp().orElse(false)).isTrue();
		assertThat(configuration.getServerRegionShortcut().orElse(null)).isEqualTo(RegionShortcut.PARTITION_PERSISTENT);

		verify(mockImportMetadata, times(1))
			.getAnnotationAttributes(eq(EnableClusterConfiguration.class.getName()));

		verify(mockImportMetadata, times(1))
			.hasAnnotation(eq(EnableClusterConfiguration.class.getName()));
	}

	@Test
	public void setImportMetadataFromProperties() {

		AnnotationMetadata mockImportMetadata = mock(AnnotationMetadata.class);

		Map<String, Object> annotationAttributes = new HashMap<>();

		annotationAttributes.put("host", "skullbox");
		annotationAttributes.put("port", 12345);
		annotationAttributes.put("enableInterceptors", false);
		annotationAttributes.put("followRedirects", false);
		annotationAttributes.put("requireHttps", true);
		annotationAttributes.put("serverRegionShortcut", RegionShortcut.PARTITION_PERSISTENT);
		annotationAttributes.put("useHttp", false);

		when(mockImportMetadata.getAnnotationAttributes(EnableClusterConfiguration.class.getName()))
			.thenReturn(annotationAttributes);

		when(mockImportMetadata.hasAnnotation(eq(EnableClusterConfiguration.class.getName()))).thenReturn(true);

		Environment mockEnvironment = mock(Environment.class);

		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.cluster.region.type"))).thenReturn(true);
		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.management.http.host"))).thenReturn(true);
		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.management.http.port"))).thenReturn(true);
		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.management.http.enable-interceptors"))).thenReturn(true);
		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.management.http.follow-redirects"))).thenReturn(true);
		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.management.require-https"))).thenReturn(true);
		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.management.use-http"))).thenReturn(true);

		when(mockEnvironment.getProperty(eq("spring.data.gemfire.cluster.region.type"), eq(RegionShortcut.class), any(RegionShortcut.class)))
			.thenReturn(RegionShortcut.LOCAL);

		when(mockEnvironment.getProperty(eq("spring.data.gemfire.management.http.host"), eq(String.class), any(String.class)))
			.thenReturn("cardboardBox");

		when(mockEnvironment.getProperty(eq("spring.data.gemfire.management.http.port"), eq(Integer.class), any(Integer.class)))
			.thenReturn(11235);

		when(mockEnvironment.getProperty(eq("spring.data.gemfire.management.http.enable-interceptors"), eq(Boolean.class), any(Boolean.class)))
			.thenReturn(true);

		when(mockEnvironment.getProperty(eq("spring.data.gemfire.management.http.follow-redirects"), eq(Boolean.class), any(Boolean.class)))
			.thenReturn(true);

		when(mockEnvironment.getProperty(eq("spring.data.gemfire.management.require-https"), eq(Boolean.class), any(Boolean.class)))
			.thenReturn(false);

		when(mockEnvironment.getProperty(eq("spring.data.gemfire.management.use-http"), eq(Boolean.class), any(Boolean.class)))
			.thenReturn(true);

		when(mockEnvironment.resolveRequiredPlaceholders(anyString()))
			.thenAnswer(invocation -> invocation.getArgument(0));

		ClusterConfigurationConfiguration configuration = new ClusterConfigurationConfiguration();

		configuration.setEnvironment(mockEnvironment);
		configuration.setImportMetadata(mockImportMetadata);

		assertThat(configuration.getManagementHttpHost().orElse(null)).isEqualTo("cardboardBox");
		assertThat(configuration.getManagementHttpPort().orElse(0)).isEqualTo(11235);
		assertThat(configuration.getManagementHttpEnableInterceptors().orElse(false)).isTrue();
		assertThat(configuration.getManagementHttpFollowRedirects().orElse(false)).isTrue();
		assertThat(configuration.getManagementRequireHttps().orElse(true)).isFalse();
		assertThat(configuration.getManagementUseHttp().orElse(false)).isTrue();
		assertThat(configuration.getServerRegionShortcut().orElse(null)).isEqualTo(RegionShortcut.LOCAL);

		verify(mockImportMetadata, times(1))
			.getAnnotationAttributes(eq(EnableClusterConfiguration.class.getName()));

		verify(mockImportMetadata, times(1))
			.hasAnnotation(eq(EnableClusterConfiguration.class.getName()));

		verify(mockEnvironment, times(1))
			.containsProperty(eq("spring.data.gemfire.cluster.region.type"));

		verify(mockEnvironment, times(1))
			.containsProperty(eq("spring.data.gemfire.management.http.host"));

		verify(mockEnvironment, times(1))
			.containsProperty(eq("spring.data.gemfire.management.http.port"));

		verify(mockEnvironment, times(1))
			.containsProperty(eq("spring.data.gemfire.management.http.enable-interceptors"));

		verify(mockEnvironment, times(1))
			.containsProperty(eq("spring.data.gemfire.management.http.follow-redirects"));

		verify(mockEnvironment, times(1))
			.containsProperty(eq("spring.data.gemfire.management.require-https"));

		verify(mockEnvironment, times(1))
			.containsProperty(eq("spring.data.gemfire.management.use-http"));

		verify(mockEnvironment, times(1))
			.getProperty(eq("spring.data.gemfire.cluster.region.type"), eq(RegionShortcut.class),
				eq(RegionShortcut.PARTITION_PERSISTENT));

		verify(mockEnvironment, times(1))
			.getProperty(eq("spring.data.gemfire.management.http.host"), eq(String.class), eq("skullbox"));

		verify(mockEnvironment, times(1))
			.getProperty(eq("spring.data.gemfire.management.http.port"), eq(Integer.class), eq(12345));

		verify(mockEnvironment, times(1))
			.getProperty(eq("spring.data.gemfire.management.http.enable-interceptors"), eq(Boolean.class), eq(false));

		verify(mockEnvironment, times(1))
			.getProperty(eq("spring.data.gemfire.management.http.follow-redirects"), eq(Boolean.class), eq(false));

		verify(mockEnvironment, times(1))
			.getProperty(eq("spring.data.gemfire.management.require-https"), eq(Boolean.class), eq(true));

		verify(mockEnvironment, times(1))
			.getProperty(eq("spring.data.gemfire.management.use-http"), eq(Boolean.class), eq(false));
	}

	@Test
	public void setImportMetadataFromAnnotationAttributesAndProperties() {

		AnnotationMetadata mockImportMetadata = mock(AnnotationMetadata.class);

		Map<String, Object> annotationAttributes = new HashMap<>();

		annotationAttributes.put("host", "postOfficeBox");
		annotationAttributes.put("port", 10101);
		annotationAttributes.put("enableInterceptors", false);
		annotationAttributes.put("followRedirects", false);
		annotationAttributes.put("requireHttps", false);
		annotationAttributes.put("serverRegionShortcut", RegionShortcut.REPLICATE);
		annotationAttributes.put("useHttp", true);

		when(mockImportMetadata.getAnnotationAttributes(EnableClusterConfiguration.class.getName()))
			.thenReturn(annotationAttributes);

		when(mockImportMetadata.hasAnnotation(eq(EnableClusterConfiguration.class.getName()))).thenReturn(true);

		Environment mockEnvironment = mock(Environment.class);

		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.cluster.region.type"))).thenReturn(false);
		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.management.http.host"))).thenReturn(true);
		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.management.http.port"))).thenReturn(true);
		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.management.http.enable-interceptors"))).thenReturn(false);
		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.management.http.follow-redirects"))).thenReturn(true);
		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.management.require-https"))).thenReturn(false);
		when(mockEnvironment.containsProperty(eq("spring.data.gemfire.management.use-http"))).thenReturn(false);

		when(mockEnvironment.getProperty(eq("spring.data.gemfire.management.http.follow-redirects"), eq(Boolean.class), any(Boolean.class)))
			.thenReturn(true);

		when(mockEnvironment.getProperty(eq("spring.data.gemfire.management.http.host"), eq(String.class), any(String.class)))
			.thenReturn("shoebox");

		when(mockEnvironment.getProperty(eq("spring.data.gemfire.management.http.port"), eq(Integer.class), any(Integer.class)))
			.thenReturn(12480);

		when(mockEnvironment.resolveRequiredPlaceholders(anyString()))
			.thenAnswer(invocation -> invocation.getArgument(0));

		ClusterConfigurationConfiguration configuration = new ClusterConfigurationConfiguration();

		configuration.setEnvironment(mockEnvironment);
		configuration.setImportMetadata(mockImportMetadata);

		assertThat(configuration.getManagementHttpHost().orElse(null)).isEqualTo("shoebox");
		assertThat(configuration.getManagementHttpPort().orElse(0)).isEqualTo(12480);
		assertThat(configuration.getManagementHttpEnableInterceptors().orElse(true)).isFalse();
		assertThat(configuration.getManagementHttpFollowRedirects().orElse(false)).isTrue();
		assertThat(configuration.getManagementRequireHttps().orElse(true)).isFalse();
		assertThat(configuration.getManagementUseHttp().orElse(false)).isTrue();
		assertThat(configuration.getServerRegionShortcut().orElse(null)).isEqualTo(RegionShortcut.REPLICATE);

		verify(mockImportMetadata, times(1))
			.getAnnotationAttributes(eq(EnableClusterConfiguration.class.getName()));

		verify(mockImportMetadata, times(1))
			.hasAnnotation(eq(EnableClusterConfiguration.class.getName()));

		verify(mockEnvironment, times(1))
			.containsProperty(eq("spring.data.gemfire.cluster.region.type"));

		verify(mockEnvironment, times(1))
			.containsProperty(eq("spring.data.gemfire.management.http.host"));

		verify(mockEnvironment, times(1))
			.containsProperty(eq("spring.data.gemfire.management.http.port"));

		verify(mockEnvironment, times(1))
			.containsProperty(eq("spring.data.gemfire.management.require-https"));

		verify(mockEnvironment, times(1))
			.containsProperty(eq("spring.data.gemfire.management.use-http"));

		verify(mockEnvironment, never())
			.getProperty(eq("spring.data.gemfire.cluster.region.type"), eq(RegionShortcut.class),
				any(RegionShortcut.class));

		verify(mockEnvironment, times(1))
			.getProperty(eq("spring.data.gemfire.management.http.host"), eq(String.class), eq("postOfficeBox"));

		verify(mockEnvironment, times(1))
			.getProperty(eq("spring.data.gemfire.management.http.port"), eq(Integer.class), eq(10101));

		verify(mockEnvironment, never())
			.getProperty(eq("spring.data.gemfire.management.http.enable-interceptors"), eq(Boolean.class), anyBoolean());

		verify(mockEnvironment, times(1))
			.getProperty(eq("spring.data.gemfire.management.http.follow-redirects"), eq(Boolean.class), eq(false));

		verify(mockEnvironment, never())
			.getProperty(eq("spring.data.gemfire.management.require-https"), eq(Boolean.class), anyBoolean());

		verify(mockEnvironment, never())
			.getProperty(eq("spring.data.gemfire.management.use-http"), eq(Boolean.class), anyBoolean());
	}

	@Test
	public void gemfireClusterSchemaObjectInitializerBeanIsCorrect() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		ClientCache mockClientCache = mock(ClientCache.class);

		Environment mockEnvironment = mock(Environment.class);

		GemfireAdminOperations mockGemfireAdminOperations = mock(GemfireAdminOperations.class);

		ClusterConfigurationConfiguration configuration = spy(new ClusterConfigurationConfiguration());

		doReturn(mockGemfireAdminOperations).when(configuration)
			.resolveGemfireAdminOperations(eq(mockEnvironment), eq(mockClientCache));

		configuration.setBeanFactory(mockBeanFactory);

		ClusterSchemaObjectInitializer initializer =
			configuration.gemfireClusterSchemaObjectInitializer(mockEnvironment, mockClientCache);

		assertThat(initializer).isNotNull();
		assertThat(initializer.isAutoStartup()).isTrue();

		SchemaObjectContext schemaObjectContext = initializer.getSchemaObjectContext();

		assertThat(schemaObjectContext).isNotNull();
		assertThat(schemaObjectContext.<GemfireAdminOperations>getGemfireAdminOperations()).isEqualTo(mockGemfireAdminOperations);
		assertThat(schemaObjectContext.getSchemaObjectCollector()).isInstanceOf(ComposableSchemaObjectCollector.class);
		assertThat(schemaObjectContext.getSchemaObjectDefiner()).isInstanceOf(ComposableSchemaObjectDefiner.class);

		verify(configuration, times(1))
			.resolveGemfireAdminOperations(eq(mockEnvironment), eq(mockClientCache));
		verify(configuration, never()).resolveClientHttpRequestInterceptors(anyBoolean());
		verify(configuration, never()).resolveRestTemplateConfigurers();
	}

	@Test
	public void gemfireClusterSchemaObjectInitializerBeanIsNullWhenGemFireCacheIsNull() {

		ClusterConfigurationConfiguration configuration = new ClusterConfigurationConfiguration();

		Environment mockEnvironment = mock(Environment.class);

		assertThat(configuration.gemfireClusterSchemaObjectInitializer(mockEnvironment, null)).isNull();
	}

	@Test
	public void gemfireClusterSchemaObjectInitializerBeanIsNullWhenGemFireCacheIsNotAClientCache() {

		Cache mockPeerCache = mock(Cache.class);

		ClusterConfigurationConfiguration configuration = new ClusterConfigurationConfiguration();

		Environment mockEnvironment = mock(Environment.class);

		assertThat(configuration.gemfireClusterSchemaObjectInitializer(mockEnvironment, mockPeerCache)).isNull();
	}

	@Test
	public void resolvesAutowiredClientHttpRequestInterceptors() throws Exception {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		ClientHttpRequestInterceptor mockInterceptorOne = mock(ClientHttpRequestInterceptor.class);
		ClientHttpRequestInterceptor mockInterceptorTwo = mock(ClientHttpRequestInterceptor.class);

		List<ClientHttpRequestInterceptor> clientHttpRequestInterceptors =
			Arrays.asList(mockInterceptorOne, mockInterceptorTwo);

		ClusterConfigurationConfiguration configuration = new ClusterConfigurationConfiguration();

		configuration = autowire(configuration, "clientHttpRequestInterceptors", clientHttpRequestInterceptors);
		configuration.setBeanFactory(mockBeanFactory);

		assertThat(configuration.resolveClientHttpRequestInterceptors(true))
			.isEqualTo(clientHttpRequestInterceptors);

		verifyZeroInteractions(mockBeanFactory);
	}

	@Test
	public void resolvesClientHttpRequestInterceptorsFromBeanFactory() {

		ListableBeanFactory mockBeanFactory = mock(ListableBeanFactory.class);

		ClientHttpRequestInterceptor mockInterceptorOne = mock(ClientHttpRequestInterceptor.class);
		ClientHttpRequestInterceptor mockInterceptorTwo = mock(ClientHttpRequestInterceptor.class);

		Map<String, ClientHttpRequestInterceptor> expectedClientHttpRequestInterceptors = new HashMap<>();

		expectedClientHttpRequestInterceptors.put("MockInterceptorOne", mockInterceptorOne);
		expectedClientHttpRequestInterceptors.put("MockInterceptorTwo", mockInterceptorTwo);

		when(mockBeanFactory.getBeansOfType(eq(ClientHttpRequestInterceptor.class), anyBoolean(), anyBoolean()))
			.thenReturn(expectedClientHttpRequestInterceptors);

		ClusterConfigurationConfiguration configuration = new ClusterConfigurationConfiguration();

		configuration.setBeanFactory(mockBeanFactory);

		List<ClientHttpRequestInterceptor> actualClientHttpRequestInterceptors =
			configuration.resolveClientHttpRequestInterceptors(true);

		assertThat(actualClientHttpRequestInterceptors).isNotNull();
		assertThat(actualClientHttpRequestInterceptors).hasSize(expectedClientHttpRequestInterceptors.size());
		assertThat(actualClientHttpRequestInterceptors)
			.containsExactlyInAnyOrder(expectedClientHttpRequestInterceptors.values()
				.toArray(new ClientHttpRequestInterceptor[0]));

		verify(mockBeanFactory, times(1))
			.getBeansOfType(eq(ClientHttpRequestInterceptor.class), eq(true), eq(false));
	}

	@Test
	public void resolveClientHttpRequestInterceptorsReturnsNullWhenBeanFactoryIsNotAListableBeanFactory() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		ClusterConfigurationConfiguration configuration = new ClusterConfigurationConfiguration();

		configuration.setBeanFactory(mockBeanFactory);

		List<ClientHttpRequestInterceptor> clientHttpRequestInterceptors =
			configuration.resolveClientHttpRequestInterceptors(true);

		assertThat(clientHttpRequestInterceptors).isNotNull();
		assertThat(clientHttpRequestInterceptors).isEmpty();

		verifyZeroInteractions(mockBeanFactory);
	}

	@Test
	public void resolvesAutowiredGemfireAdminOperations() throws Exception {

		GemfireAdminOperations mockGemfireAdminOperations = mock(GemfireAdminOperations.class);

		ClusterConfigurationConfiguration configuration = new ClusterConfigurationConfiguration();

		configuration = autowire(configuration, "gemfireAdminOperations", mockGemfireAdminOperations);

		assertThat(configuration.resolveGemfireAdminOperations(null, null))
			.isSameAs(mockGemfireAdminOperations);
	}

	@Test
	public void resolvesNewFunctionGemfireAdminOperations() {

		ClientCache mockClientCache = mock(ClientCache.class);

		ClusterConfigurationConfiguration configuration = new ClusterConfigurationConfiguration();

		GemfireAdminOperations operations =
			configuration.resolveGemfireAdminOperations(null, mockClientCache);

		assertThat(operations).isInstanceOf(FunctionGemfireAdminTemplate.class);

		verifyZeroInteractions(mockClientCache);
	}

	@Test
	public void resolvesNewRestHttpGemfireAdminOperationsUsingDefaults() throws Exception {

		ClientCache mockClientCache = mock(ClientCache.class);

		ClusterConfigurationConfiguration configuration = spy(new ClusterConfigurationConfiguration());

		doReturn(Collections.emptyList()).when(configuration).resolveClientHttpRequestInterceptors(anyBoolean());
		doReturn(Collections.emptyList()).when(configuration).resolveRestTemplateConfigurers();

		Environment mockEnvironment = mock(Environment.class);

		when(mockEnvironment.getProperty(anyString(), eq(Boolean.class), anyBoolean())).thenReturn(false);

		configuration.setManagementUseHttp(true);

		assertThat(configuration.resolveManagementRequireHttps()).isTrue();
		assertThat(configuration.resolveManagementUseHttp()).isTrue();

		GemfireAdminOperations operations =
			configuration.resolveGemfireAdminOperations(mockEnvironment, mockClientCache);

		assertThat(operations).isInstanceOf(RestHttpGemfireAdminTemplate.class);

		RestHttpGemfireAdminTemplate template = (RestHttpGemfireAdminTemplate) operations;

		RestTemplate restTemplate = getFieldValue(template, "restTemplate");

		assertThat(restTemplate).isNotNull();
		assertThat(restTemplate.getInterceptors()).isEmpty();
		assertThat(restTemplate.getRequestFactory()).isInstanceOf(FollowRedirectsSimpleClientHttpRequestFactory.class);

		FollowRedirectsSimpleClientHttpRequestFactory clientHttpRequestFactory =
			(FollowRedirectsSimpleClientHttpRequestFactory) restTemplate.getRequestFactory();

		assertThat(clientHttpRequestFactory.isFollowRedirects()).isFalse();

		verifyZeroInteractions(mockClientCache);
		verify(configuration, times(1)).resolveClientHttpRequestInterceptors(eq(false));
		verify(configuration, times(1)).resolveRestTemplateConfigurers();
	}

	@Test
	public void resolvesNewRestHttpGemfireAdminOperationsSetsFollowRedirectsWithProperty() throws Exception {

		ClientCache mockClientCache = mock(ClientCache.class);

		ClusterConfigurationConfiguration configuration = spy(new ClusterConfigurationConfiguration());

		doReturn(Collections.emptyList()).when(configuration).resolveClientHttpRequestInterceptors(anyBoolean());
		doReturn(Collections.emptyList()).when(configuration).resolveRestTemplateConfigurers();

		Environment environment = spy(new StandardEnvironment());

		configuration.setManagementHttpFollowRedirects(true);
		configuration.setManagementUseHttp(true);

		assertThat(configuration.resolveManagementHttpFollowRedirects()).isTrue();
		assertThat(configuration.resolveManagementRequireHttps()).isTrue();
		assertThat(configuration.resolveManagementUseHttp()).isTrue();

		GemfireAdminOperations operations = configuration.resolveGemfireAdminOperations(environment, mockClientCache);

		assertThat(operations).isInstanceOf(RestHttpGemfireAdminTemplate.class);

		RestHttpGemfireAdminTemplate template = (RestHttpGemfireAdminTemplate) operations;

		RestTemplate restTemplate = getFieldValue(template, "restTemplate");

		assertThat(restTemplate).isNotNull();
		assertThat(restTemplate.getInterceptors()).isEmpty();
		assertThat(restTemplate.getRequestFactory()).isInstanceOf(FollowRedirectsSimpleClientHttpRequestFactory.class);

		FollowRedirectsSimpleClientHttpRequestFactory clientHttpRequestFactory =
			(FollowRedirectsSimpleClientHttpRequestFactory) restTemplate.getRequestFactory();

		assertThat(clientHttpRequestFactory.isFollowRedirects()).isTrue();

		verifyZeroInteractions(mockClientCache);
		verify(configuration, times(1)).resolveClientHttpRequestInterceptors(eq(false));
		verify(configuration, times(1)).resolveRestTemplateConfigurers();
	}

	@Test
	public void resolvesNewRestHttpGemfireAdminOperationsUsesClientHttpRequestInterceptorsSetsFollowRedirectsWhenUsingHttp()
			throws Exception {

		assertThat(Boolean.getBoolean(ClusterConfigurationConfiguration.HTTP_FOLLOW_REDIRECTS_PROPERTY)).isFalse();

		ClientCache mockClientCache = mock(ClientCache.class);

		ClientHttpRequestInterceptor mockInterceptorOne = mock(ClientHttpRequestInterceptor.class);
		ClientHttpRequestInterceptor mockInterceptorTwo = mock(ClientHttpRequestInterceptor.class);

		ClusterConfigurationConfiguration configuration = spy(new ClusterConfigurationConfiguration());

		Environment environment = spy(new StandardEnvironment());

		doReturn(Arrays.asList(mockInterceptorOne, mockInterceptorTwo))
			.when(configuration).resolveClientHttpRequestInterceptors(anyBoolean());

		doReturn(Collections.emptyList()).when(configuration).resolveRestTemplateConfigurers();

		configuration.setManagementHttpEnableInterceptors(true);
		configuration.setManagementRequireHttps(false);
		configuration.setManagementUseHttp(true);

		assertThat(configuration.resolveManagementRequireHttps()).isFalse();
		assertThat(configuration.resolveManagementUseHttp()).isTrue();

		GemfireAdminOperations operations = configuration.resolveGemfireAdminOperations(environment, mockClientCache);

		assertThat(operations).isInstanceOf(RestHttpGemfireAdminTemplate.class);

		RestHttpGemfireAdminTemplate template = (RestHttpGemfireAdminTemplate) operations;

		RestTemplate restTemplate = getFieldValue(template, "restTemplate");

		assertThat(restTemplate).isNotNull();
		assertThat(restTemplate.getInterceptors()).containsExactly(mockInterceptorOne, mockInterceptorTwo);
		assertThat(restTemplate.getRequestFactory()).isInstanceOf(InterceptingClientHttpRequestFactory.class);

		FollowRedirectsSimpleClientHttpRequestFactory clientHttpRequestFactory =
			getFieldValue(restTemplate.getRequestFactory(), "requestFactory");

		assertThat(clientHttpRequestFactory.isFollowRedirects()).isTrue();

		verifyZeroInteractions(mockClientCache);
		verify(configuration, times(1)).resolveClientHttpRequestInterceptors(eq(true));
		verify(configuration, times(1)).resolveRestTemplateConfigurers();
	}

	@Test
	public void resolvesNewRestHttpGemfireAdminOperationsAvoidsSettingInvalidPort() throws Exception {

		ClientCache mockClientCache = mock(ClientCache.class);

		ClusterConfigurationConfiguration configuration = spy(new ClusterConfigurationConfiguration());

		doReturn(Collections.emptyList()).when(configuration).resolveClientHttpRequestInterceptors(anyBoolean());
		doReturn(Collections.emptyList()).when(configuration).resolveRestTemplateConfigurers();

		Environment environment = mock(Environment.class);

		when(environment.getProperty(anyString(), eq(Boolean.class), anyBoolean())).thenReturn(false);

		configuration.setManagementHttpHost("skullbox");
		configuration.setManagementHttpPort(-1);
		configuration.setManagementUseHttp(true);

		assertThat(configuration.resolveManagementHttpHost()).isEqualTo("skullbox");
		assertThat(configuration.resolveManagementHttpPort()).isEqualTo(-1);
		assertThat(configuration.resolveManagementUseHttp()).isTrue();

		GemfireAdminOperations operations = configuration.resolveGemfireAdminOperations(environment, mockClientCache);

		assertThat(operations).isInstanceOf(RestHttpGemfireAdminTemplate.class);

		RestHttpGemfireAdminTemplate template = (RestHttpGemfireAdminTemplate) operations;

		assertThat(this.<String>getFieldValue(template, "managementRestApiUrl"))
			.isEqualTo("https://skullbox/gemfire/v1");

		verifyZeroInteractions(mockClientCache);
		verify(configuration, times(1)).resolveClientHttpRequestInterceptors(eq(false));
		verify(configuration, times(1)).resolveRestTemplateConfigurers();
	}
}
