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
package org.springframework.data.gemfire.config.admin.remote;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.config.admin.remote.RestHttpGemfireAdminTemplate.FollowRedirectsSimpleClientHttpRequestFactory;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.query.Index;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import org.springframework.data.gemfire.IndexType;
import org.springframework.data.gemfire.config.schema.definitions.IndexDefinition;
import org.springframework.data.gemfire.config.schema.definitions.RegionDefinition;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.ClientHttpRequestFactory;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.InterceptingClientHttpRequestFactory;
import org.springframework.util.MultiValueMap;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.client.RestOperations;
import org.springframework.web.client.RestTemplate;

/**
 * Unit tests for {@link RestHttpGemfireAdminTemplate}.
 *
 * @author John Blum
 * @see java.net.URI
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.query.Index
 * @see org.springframework.data.gemfire.config.admin.remote.RestHttpGemfireAdminTemplate
 * @see org.springframework.http.HttpHeaders
 * @see org.springframework.http.client.ClientHttpRequestFactory
 * @see org.springframework.http.client.ClientHttpRequestInterceptor
 * @see org.springframework.http.client.InterceptingClientHttpRequestFactory
 * @see org.springframework.web.client.RestOperations
 * @see org.springframework.web.client.RestTemplate
 * @since 2.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class RestHttpGemfireAdminTemplateUnitTests {

	@Mock
	private ClientCache mockClientCache;

	@Mock
	private Index mockIndex;

	@Mock
	private Region mockRegion;

	private RestHttpGemfireAdminTemplate template;

	@Mock
	private RestOperations mockRestOperations;

	@Before
	public void setup() {

		this.template = new RestHttpGemfireAdminTemplate(this.mockClientCache) {

			@Override
			@SuppressWarnings("unchecked")
			protected <T extends RestOperations> T newRestOperations(ClientHttpRequestFactory clientHttpRequestFactory,
					List<ClientHttpRequestInterceptor> clientHttpRequestInterceptors) {

				return (T) mockRestOperations;
			}
		};

		when(this.mockRegion.getName()).thenReturn("MockRegion");
		when(this.mockIndex.getType()).thenReturn(IndexType.FUNCTIONAL.getGemfireIndexType());
		when(this.mockIndex.getName()).thenReturn("MockIndex");
		when(this.mockIndex.getIndexedExpression()).thenReturn("age");
		when(this.mockIndex.getFromClause()).thenReturn("/Customers");
	}

	@SuppressWarnings("unchecked")
	private <T> T getFieldValue(Object target, String fieldName) throws NoSuchFieldException {

		Field field = ReflectionUtils.findField(target.getClass(), fieldName, ClientHttpRequestFactory.class);

		return Optional.ofNullable(field)
			.map(it -> {
				ReflectionUtils.makeAccessible(it);
				return field;
			})
			.map(it -> (T) ReflectionUtils.getField(it, target))
			.orElseThrow(() ->
				new NoSuchFieldException(String.format("Field [%s] was not found on Object of type [%s]",
					fieldName, target.getClass().getName())));
	}

	@Test
	public void constructDefaultRestHttpGemfireAdminTemplate() {

		RestHttpGemfireAdminTemplate template = new RestHttpGemfireAdminTemplate(this.mockClientCache);

		assertThat(template).isNotNull();
		assertThat(template.getClientCache()).isSameAs(this.mockClientCache);
		assertThat(template.getManagementRestApiUrl())
			.isEqualTo(String.format(RestHttpGemfireAdminTemplate.MANAGEMENT_REST_API_NO_PORT_URL_TEMPLATE,
				RestHttpGemfireAdminTemplate.DEFAULT_SCHEME, RestHttpGemfireAdminTemplate.DEFAULT_HOST));
		assertThat(template.<RestOperations>getRestOperations()).isInstanceOf(RestTemplate.class);

		RestTemplate restTemplate = template.getRestOperations();

		ClientHttpRequestFactory clientHttpRequestFactory = restTemplate.getRequestFactory();

		assertThat(clientHttpRequestFactory).isInstanceOf(FollowRedirectsSimpleClientHttpRequestFactory.class);
		assertThat(((FollowRedirectsSimpleClientHttpRequestFactory) clientHttpRequestFactory).isFollowRedirects())
			.isTrue();

		List<ClientHttpRequestInterceptor> clientHttpRequestInterceptors = restTemplate.getInterceptors();

		assertThat(clientHttpRequestInterceptors).isNotNull();
		assertThat(clientHttpRequestInterceptors).isEmpty();
	}

	@Test
	@SuppressWarnings("all")
	public void constructCustomRestHttpGemfireAdminTemplate() throws Exception {

		ClientHttpRequestInterceptor mockInterceptor = mock(ClientHttpRequestInterceptor.class);

		RestHttpGemfireAdminTemplate template =
			new RestHttpGemfireAdminTemplate(this.mockClientCache, "sftp", "skullbox", 8080,
				false, Collections.singletonList(mockInterceptor));

		assertThat(template).isNotNull();
		assertThat(template.getClientCache()).isSameAs(this.mockClientCache);
		assertThat(template.getManagementRestApiUrl())
			.isEqualTo(String.format(RestHttpGemfireAdminTemplate.MANAGEMENT_REST_API_URL_TEMPLATE,
				"sftp", "skullbox", 8080));
		assertThat(template.<RestOperations>getRestOperations()).isInstanceOf(RestTemplate.class);

		RestTemplate restTemplate = (RestTemplate) template.getRestOperations();

		ClientHttpRequestFactory clientHttpRequestFactory = restTemplate.getRequestFactory();

		assertThat(clientHttpRequestFactory).isInstanceOf(InterceptingClientHttpRequestFactory.class);

		clientHttpRequestFactory = this.getFieldValue(clientHttpRequestFactory, "requestFactory");

		assertThat(clientHttpRequestFactory).isInstanceOf(FollowRedirectsSimpleClientHttpRequestFactory.class);
		assertThat(((FollowRedirectsSimpleClientHttpRequestFactory) clientHttpRequestFactory).isFollowRedirects())
			.isFalse();

		List<ClientHttpRequestInterceptor> clientHttpRequestInterceptors = restTemplate.getInterceptors();

		assertThat(clientHttpRequestInterceptors).isNotNull();
		assertThat(clientHttpRequestInterceptors).contains(mockInterceptor);
	}

	@Test
	public void newClientHttpRequestFactoryFollowsRedirectsIsTrue() {

		ClientHttpRequestFactory clientHttpRequestFactory =
			this.template.newClientHttpRequestFactory(true);

		assertThat(clientHttpRequestFactory).isInstanceOf(FollowRedirectsSimpleClientHttpRequestFactory.class);
		assertThat(((FollowRedirectsSimpleClientHttpRequestFactory) clientHttpRequestFactory).isFollowRedirects())
			.isTrue();
	}

	@Test
	public void newClientHttpRequestFactoryFollowsRedirectsIsFalse() {

		ClientHttpRequestFactory clientHttpRequestFactory =
			this.template.newClientHttpRequestFactory(false);

		assertThat(clientHttpRequestFactory).isInstanceOf(FollowRedirectsSimpleClientHttpRequestFactory.class);
		assertThat(((FollowRedirectsSimpleClientHttpRequestFactory) clientHttpRequestFactory).isFollowRedirects())
			.isFalse();
	}

	@Test
	public void newRestOperationsWithInterceptors() {

		ClientHttpRequestFactory mockClientHttpRequestFactory = mock(ClientHttpRequestFactory.class);

		ClientHttpRequestInterceptor mockInterceptorOne = mock(ClientHttpRequestInterceptor.class);
		ClientHttpRequestInterceptor mockInterceptorTwo = mock(ClientHttpRequestInterceptor.class);

		RestTemplate restTemplate = new RestHttpGemfireAdminTemplate(this.mockClientCache)
			.newRestOperations(mockClientHttpRequestFactory, Arrays.asList(mockInterceptorOne, mockInterceptorTwo));

		assertThat(restTemplate).isNotNull();
		assertThat(restTemplate.getInterceptors()).containsExactly(mockInterceptorOne, mockInterceptorTwo);
		assertThat(restTemplate.getRequestFactory()).isNotSameAs(mockClientHttpRequestFactory);
		assertThat(restTemplate.getRequestFactory()).isInstanceOf(InterceptingClientHttpRequestFactory.class);
	}

	@Test
	public void newRestOperationsWithNoInterceptors() {

		ClientHttpRequestFactory mockClientHttpRequestFactory = mock(ClientHttpRequestFactory.class);

		RestTemplate restTemplate = new RestHttpGemfireAdminTemplate(this.mockClientCache)
			.newRestOperations(mockClientHttpRequestFactory, Collections.emptyList());

		assertThat(restTemplate).isNotNull();
		assertThat(restTemplate.getInterceptors()).isEmpty();
		assertThat(restTemplate.getRequestFactory()).isSameAs(mockClientHttpRequestFactory);
	}

	@Test
	public void resolvesManagementRestApiUrlCorrectly() {

		assertThat(this.template.resolveManagementRestApiUrl("http", "boombox", 80))
			.isEqualTo(String.format(RestHttpGemfireAdminTemplate.MANAGEMENT_REST_API_URL_TEMPLATE,
				"http", "boombox", 80));

		assertThat(this.template.resolveManagementRestApiUrl("https", "cardboardbox", 443))
			.isEqualTo(String.format(RestHttpGemfireAdminTemplate.MANAGEMENT_REST_API_URL_TEMPLATE,
				"https", "cardboardbox", 443));

		assertThat(this.template.resolveManagementRestApiUrl("ftp", "lunchbox", 21))
			.isEqualTo(String.format(RestHttpGemfireAdminTemplate.MANAGEMENT_REST_API_URL_TEMPLATE,
				"ftp", "lunchbox", 21));

		assertThat(this.template.resolveManagementRestApiUrl("sftp", "mailbox", 22))
			.isEqualTo(String.format(RestHttpGemfireAdminTemplate.MANAGEMENT_REST_API_URL_TEMPLATE,
				"sftp", "mailbox", 22));

		assertThat(this.template.resolveManagementRestApiUrl("smtp", "skullbox", 25))
			.isEqualTo(String.format(RestHttpGemfireAdminTemplate.MANAGEMENT_REST_API_URL_TEMPLATE,
				"smtp", "skullbox", 25));
	}

	@Test
	public void resolvesManagementRestApiUrlCorrectlyWhenInvalidPortIsGiven() {

		assertThat(this.template.resolveManagementRestApiUrl("https", "box", -1))
			.isEqualTo(String.format(RestHttpGemfireAdminTemplate.MANAGEMENT_REST_API_NO_PORT_URL_TEMPLATE,
				"https", "box"));

		assertThat(this.template.resolveManagementRestApiUrl("http", "dropbox", 0))
			.isEqualTo(String.format(RestHttpGemfireAdminTemplate.MANAGEMENT_REST_API_NO_PORT_URL_TEMPLATE,
				"http", "dropbox"));

		assertThat(this.template.resolveManagementRestApiUrl("https", "jambox", 65536))
			.isEqualTo(String.format(RestHttpGemfireAdminTemplate.MANAGEMENT_REST_API_NO_PORT_URL_TEMPLATE,
				"https", "jambox"));

		assertThat(this.template.resolveManagementRestApiUrl("http", "shoebox", 101123))
			.isEqualTo(String.format(RestHttpGemfireAdminTemplate.MANAGEMENT_REST_API_NO_PORT_URL_TEMPLATE,
				"http", "shoebox"));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void createIndexCallsGemFireManagementRestApi() {

		IndexDefinition indexDefinition = IndexDefinition.from(this.mockIndex);

		when(this.mockRestOperations.exchange(any(RequestEntity.class), eq(String.class))).thenAnswer(invocation -> {

			RequestEntity requestEntity = invocation.getArgument(0);

			assertThat(requestEntity).isNotNull();
			assertThat(requestEntity.getMethod()).isEqualTo(HttpMethod.POST);
			assertThat(requestEntity.getUrl()).isEqualTo(URI.create("https://localhost/gemfire/v1/indexes"));

			HttpHeaders headers = requestEntity.getHeaders();

			assertThat(headers).isNotNull();
			assertThat(headers.getContentType()).isEqualTo(MediaType.APPLICATION_FORM_URLENCODED);

			Object body = requestEntity.getBody();

			assertThat(body).isInstanceOf(MultiValueMap.class);

			MultiValueMap<String, Object> requestBody = (MultiValueMap<String, Object>) body;

			assertThat(requestBody).isNotNull();
			assertThat(requestBody.getFirst("name")).isEqualTo(indexDefinition.getName());
			assertThat(requestBody.getFirst("expression")).isEqualTo(indexDefinition.getExpression());
			assertThat(requestBody.getFirst("region")).isEqualTo(indexDefinition.getFromClause());
			assertThat(requestBody.getFirst("type")).isEqualTo(indexDefinition.getIndexType().toString());

			return new ResponseEntity(HttpStatus.OK);
		});

		this.template.createIndex(indexDefinition);

		verify(this.mockRestOperations, times(1))
			.exchange(isA(RequestEntity.class), eq(String.class));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void createRegionCallsGemFireManagementRestApi() {

		RegionDefinition regionDefinition = RegionDefinition.from(this.mockRegion);

		when(this.mockRestOperations.exchange(any(RequestEntity.class), eq(String.class))).thenAnswer(invocation -> {

			RequestEntity requestEntity = invocation.getArgument(0);

			assertThat(requestEntity).isNotNull();
			assertThat(requestEntity.getMethod()).isEqualTo(HttpMethod.POST);
			assertThat(requestEntity.getUrl()).isEqualTo(URI.create("https://localhost/gemfire/v1/regions"));

			HttpHeaders headers = requestEntity.getHeaders();

			assertThat(headers).isNotNull();
			assertThat(headers.getContentType()).isEqualTo(MediaType.APPLICATION_FORM_URLENCODED);

			Object body = requestEntity.getBody();

			assertThat(body).isInstanceOf(MultiValueMap.class);

			MultiValueMap<String, Object> requestBody = (MultiValueMap<String, Object>) body;

			assertThat(requestBody).isNotNull();
			assertThat(requestBody.getFirst("name")).isEqualTo(regionDefinition.getName());
			assertThat(requestBody.getFirst("type")).isEqualTo(regionDefinition.getRegionShortcut().toString());
			assertThat(requestBody.getFirst("skip-if-exists"))
				.isEqualTo(String.valueOf(RestHttpGemfireAdminTemplate.DEFAULT_CREATE_REGION_SKIP_IF_EXISTS));

			return new ResponseEntity(HttpStatus.OK);
		});

		this.template.createRegion(regionDefinition);

		verify(this.mockRestOperations, times(1))
			.exchange(isA(RequestEntity.class), eq(String.class));
	}
}
