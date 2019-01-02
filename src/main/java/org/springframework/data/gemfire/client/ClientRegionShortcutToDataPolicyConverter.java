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

import java.util.Optional;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.springframework.core.convert.converter.Converter;
import org.springframework.lang.Nullable;

/**
 * Spring {@link Converter} to convert a {@link ClientRegionShortcut} into a {@link DataPolicy}.
 *
 * @author John Blum
 * @see org.apache.geode.cache.DataPolicy
 * @see org.apache.geode.cache.client.ClientRegionShortcut
 * @see org.springframework.core.convert.converter.Converter
 * @see org.springframework.data.gemfire.client.ClientRegionShortcutWrapper
 * @since 2.0.2
 */
public class ClientRegionShortcutToDataPolicyConverter implements Converter<ClientRegionShortcut, DataPolicy> {

	public static final ClientRegionShortcutToDataPolicyConverter INSTANCE =
		new ClientRegionShortcutToDataPolicyConverter();

	/**
	 * Converts the given {@link ClientRegionShortcut} into a corresponding {@link DataPolicy}.
	 *
	 * @param regionShortcut {@link ClientRegionShortcut} to convert.
	 * @return a corresponding {@link DataPolicy} for the given {@link ClientRegionShortcut}.
	 * @see org.apache.geode.cache.client.ClientRegionShortcut
	 * @see org.apache.geode.cache.DataPolicy
	 */
	@Nullable @Override
	public DataPolicy convert(ClientRegionShortcut clientRegionShortcut) {

		return Optional.ofNullable(ClientRegionShortcutWrapper.valueOf(clientRegionShortcut))
			.map(ClientRegionShortcutWrapper::getDataPolicy)
			.orElse(DataPolicy.DEFAULT);
	}
}
