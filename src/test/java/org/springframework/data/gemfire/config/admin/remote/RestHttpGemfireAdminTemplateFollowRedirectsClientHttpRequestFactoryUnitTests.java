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
package org.springframework.data.gemfire.config.admin.remote;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.springframework.data.gemfire.config.admin.remote.RestHttpGemfireAdminTemplate.FollowRedirectsSimpleClientHttpRequestFactory;

import java.io.IOException;
import java.net.HttpURLConnection;

import org.junit.Test;
import org.mockito.InOrder;
import org.mockito.Mockito;

/**
 * Unit Tests for {@link RestHttpGemfireAdminTemplate.FollowRedirectsSimpleClientHttpRequestFactory}.
 *
 * @author John Blum
 * @see java.net.HttpURLConnection
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.config.admin.remote.RestHttpGemfireAdminTemplate.FollowRedirectsSimpleClientHttpRequestFactory
 * @since 2.2.0
 */
public class RestHttpGemfireAdminTemplateFollowRedirectsClientHttpRequestFactoryUnitTests {

	@Test
	public void doesNotFollowRedirectsClientHttpRequestFactory() throws IOException {

		FollowRedirectsSimpleClientHttpRequestFactory clientHttpRequestFactory =
			new FollowRedirectsSimpleClientHttpRequestFactory(false);

		assertThat(clientHttpRequestFactory).isNotNull();
		assertThat(clientHttpRequestFactory.isFollowRedirects()).isFalse();

		HttpURLConnection mockHttpUrlConnection = mock(HttpURLConnection.class);

		doCallRealMethod().when(mockHttpUrlConnection).setInstanceFollowRedirects(anyBoolean());
		doCallRealMethod().when(mockHttpUrlConnection).getInstanceFollowRedirects();

		clientHttpRequestFactory.prepareConnection(mockHttpUrlConnection, "GET");

		assertThat(mockHttpUrlConnection.getInstanceFollowRedirects()).isFalse();

		InOrder inOrder = Mockito.inOrder(mockHttpUrlConnection);

		inOrder.verify(mockHttpUrlConnection, times(1))
			.setInstanceFollowRedirects(eq(true));

		inOrder.verify(mockHttpUrlConnection, times(1))
			.setInstanceFollowRedirects(eq(false));
	}

	@Test
	public void followsRedirectsClientHttpRequestFactory() throws IOException {

		FollowRedirectsSimpleClientHttpRequestFactory clientHttpRequestFactory =
			new FollowRedirectsSimpleClientHttpRequestFactory(true);

		assertThat(clientHttpRequestFactory).isNotNull();
		assertThat(clientHttpRequestFactory.isFollowRedirects()).isTrue();

		HttpURLConnection mockHttpUrlConnection = mock(HttpURLConnection.class);

		doCallRealMethod().when(mockHttpUrlConnection).setInstanceFollowRedirects(anyBoolean());
		doCallRealMethod().when(mockHttpUrlConnection).getInstanceFollowRedirects();

		clientHttpRequestFactory.prepareConnection(mockHttpUrlConnection, "POST");

		assertThat(mockHttpUrlConnection.getInstanceFollowRedirects()).isTrue();

		InOrder inOrder = Mockito.inOrder(mockHttpUrlConnection);

		inOrder.verify(mockHttpUrlConnection, times(1))
			.setInstanceFollowRedirects(eq(false));

		inOrder.verify(mockHttpUrlConnection, times(1))
			.setInstanceFollowRedirects(eq(true));
	}
}
